/*
 * Copyright 2010-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE file.
 */
package org.jetbrains.kotlin.backend.konan.ir.interop

import org.jetbrains.kotlin.backend.konan.descriptors.findPackage
import org.jetbrains.kotlin.backend.konan.descriptors.isFromInteropLibrary
import org.jetbrains.kotlin.backend.konan.isObjCClass
import org.jetbrains.kotlin.descriptors.CallableMemberDescriptor
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.DeclarationDescriptor
import org.jetbrains.kotlin.descriptors.PackageFragmentDescriptor
import org.jetbrains.kotlin.ir.UNDEFINED_OFFSET
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.declarations.impl.IrFileImpl
import org.jetbrains.kotlin.ir.declarations.lazy.*
import org.jetbrains.kotlin.ir.symbols.*
import org.jetbrains.kotlin.ir.util.*
import org.jetbrains.kotlin.resolve.descriptorUtil.getAllSuperClassifiers
import org.jetbrains.kotlin.resolve.descriptorUtil.module

/**
 * Generates external IR declarations for descriptors from interop libraries.
 * [isSpecialInteropCase] allows to delegate providing of interop symbols to other providers.
 * For example, for CEnums we need to generate non-lazy IR.
 */
class IrProviderForInteropStubs(
        private val isSpecialInteropCase: (IrSymbol) -> Boolean
) : LazyIrProvider {

    override lateinit var declarationStubGenerator: DeclarationStubGenerator

    // We need to collect provided Objective-C classes to be able to generate RTTI for them.
    // TODO: Unify with BuiltInFictitiousFunctionIrClassFactory?
    private val filesMap = mutableMapOf<PackageFragmentDescriptor, IrFile>()

    var module: IrModuleFragment? = null
        set(value) {
            if (value == null)
                error("Provide a valid non-null module")
            if (field != null)
                error("Module has already been set")
            field = value
            value.files += filesMap.values
        }

    override fun getDeclaration(symbol: IrSymbol): IrLazyDeclarationBase? =
            when {
                // TODO: LazyIrProvider appears to be a bad interface.
                //  Incoming symbol might be already bound and we need to return its current owner.
                //  The problem is that it isn't necessary a LazyIr declaration.
                //  So for now we relate on correct behavior of subsequent providers.
                symbol.isBound -> null
                isSpecialInteropCase(symbol) -> null
                symbol.descriptor.module.isFromInteropLibrary() -> provideIrDeclaration(symbol)
                else -> null
            }

    private fun provideIrDeclaration(symbol: IrSymbol): IrLazyDeclarationBase = when (symbol) {
        is IrSimpleFunctionSymbol -> provideIrFunction(symbol)
        is IrPropertySymbol -> provideIrProperty(symbol)
        is IrTypeAliasSymbol -> provideIrTypeAlias(symbol)
        is IrClassSymbol -> provideIrClass(symbol).also {
            // We need to use extension for a descriptor because IR-based function involves
            // LazyTable referencing.
            if (it.descriptor.isObjCClass()) collectObjCClasses(it)
        }
        is IrConstructorSymbol -> provideIrConstructor(symbol)
        is IrFieldSymbol -> provideIrField(symbol)
        else -> error("Unsupported interop declaration: symbol=$symbol, descriptor=${symbol.descriptor}")
    }

    private fun referenceSuperClassifiers(descriptor: ClassDescriptor) {
        val symbolTable = declarationStubGenerator.symbolTable
        descriptor.getAllSuperClassifiers().forEach { symbolTable.referenceClassifier(it) }
    }

    private fun collectObjCClasses(klass: IrClass) {
        val descriptor = klass.descriptor
        // Force-load types to be able to generate RTTI for the given class.
        // We do it via descriptors and symbolTable to avoid creating of unnecessary and incorrect lazy classes.
        referenceSuperClassifiers(descriptor)
        descriptor.companionObjectDescriptor?.let { referenceSuperClassifiers(it) }
        // RTTI for companions will be generated when we recursively visit their parents in RTTIGenerator.
        if (!descriptor.isCompanionObject) {
            val packageFragmentDescriptor = descriptor.findPackage()
            val file = filesMap.getOrPut(packageFragmentDescriptor) {
                IrFileImpl(NaiveSourceBasedFileEntryImpl("ObjCClasses"), packageFragmentDescriptor).also {
                    this.module?.files?.add(it)
                }
            }
            klass.parent = file
            file.declarations += klass
        }
    }

    private fun provideIrFunction(symbol: IrSimpleFunctionSymbol): IrLazyFunction {
        val origin = computeOrigin(symbol.descriptor)
        return declarationStubGenerator.symbolTable.declareSimpleFunction(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol.descriptor
        ) {
            IrLazyFunction(
                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, it,
                    declarationStubGenerator, declarationStubGenerator.typeTranslator
            )
        } as IrLazyFunction
    }

    private fun provideIrProperty(symbol: IrPropertySymbol): IrLazyProperty {
        val origin = computeOrigin(symbol.descriptor)
        return declarationStubGenerator.symbolTable.declareProperty(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol.descriptor
        ) {
            IrLazyProperty(
                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol,
                    declarationStubGenerator, declarationStubGenerator.typeTranslator, null
            )
        } as IrLazyProperty
    }

    private fun provideIrClass(symbol: IrClassSymbol): IrLazyClass {
        val origin = computeOrigin(symbol.descriptor)
        return declarationStubGenerator.symbolTable.declareClass(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol.descriptor
        ) {
            IrLazyClass(
                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol,
                    declarationStubGenerator, declarationStubGenerator.typeTranslator
            )
        } as IrLazyClass
    }

    private fun provideIrConstructor(symbol: IrConstructorSymbol): IrLazyConstructor {
        val origin = computeOrigin(symbol.descriptor)
        return declarationStubGenerator.symbolTable.declareConstructor(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol.descriptor
        ) {
            IrLazyConstructor(
                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol,
                    declarationStubGenerator, declarationStubGenerator.typeTranslator
            )
        } as IrLazyConstructor
    }

    private fun provideIrTypeAlias(symbol: IrTypeAliasSymbol): IrLazyTypeAlias =
            declarationStubGenerator.symbolTable.declareTypeAlias(symbol.descriptor) {
                IrLazyTypeAlias(
                        UNDEFINED_OFFSET, UNDEFINED_OFFSET,
                        computeOrigin(symbol.descriptor),
                        symbol, symbol.descriptor.name,
                        symbol.descriptor.visibility, symbol.descriptor.isActual,
                        declarationStubGenerator, declarationStubGenerator.typeTranslator
                )
            } as IrLazyTypeAlias

    private fun provideIrField(symbol: IrFieldSymbol): IrLazyField {
        val type = declarationStubGenerator.typeTranslator.translateType(symbol.descriptor.type)
        val origin = computeOrigin(symbol.descriptor)
        return declarationStubGenerator.symbolTable.declareField(
                UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin, symbol.descriptor, type
        ) {
            IrLazyField(
                    UNDEFINED_OFFSET, UNDEFINED_OFFSET, origin,
                    symbol, declarationStubGenerator, declarationStubGenerator.typeTranslator
            )
        } as IrLazyField
    }

    private fun computeOrigin(descriptor: DeclarationDescriptor): IrDeclarationOrigin {
        val nonDefaultOrigin = when (descriptor) {
            is CallableMemberDescriptor -> if (descriptor.kind == CallableMemberDescriptor.Kind.FAKE_OVERRIDE)
                IrDeclarationOrigin.FAKE_OVERRIDE else null
            else -> null
        }
        return nonDefaultOrigin ?: IrDeclarationOrigin.IR_EXTERNAL_DECLARATION_STUB
    }

}