<template>
  <span class="invoice-type-select flex-rsc child-mr-10">
    <common-select
      v-model="copyInvoiceType"
      :options="invoiceTypeEnum.ENUM"
      type="enum"
      :unshowOptions="props.unshowOptions"
      :clearable="props.clearable"
      :disabled="props.disabled"
      style="width: 150px"
      placeholder="选择票据类型"
      @change="handleInvoiceTypeChange"
    />
    <template v-if="copyInvoiceType !== invoiceTypeEnum.RECEIPT.V">
      <el-checkbox v-if="showChecked" v-model="copyChecked" label="免税" size="large" @change="checkChange" />
      <common-select
        v-model="copyTaxRate"
        :options="taxRateOption"
        :disabled="props.disabled || copyChecked"
        :data-structure="{ key: 'value', label: 'value', value: 'value' }"
        allow-create
        style="width: 80px"
        :clearable="props.clearable"
        filterable
        placeholder="税率"
        @blur="selectBlur"
        @change="handleTaxRateChange"
      />
      <span>%</span>
    </template>
  </span>
</template>

<script setup>
import { defineProps, defineEmits, ref, watchEffect } from 'vue'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { isBlank } from '@/utils/data-type'

const emit = defineEmits(['update:invoiceType', 'update:taxRate', 'update:checked', 'change'])

const props = defineProps({
  invoiceType: {
    type: Number
  },
  taxRate: {
    type: Number
  },
  checked: {
    type: Boolean
  },
  showChecked: {
    type: Boolean,
    default: false
  },
  taxRateOption: {
    type: Array,
    default: () => []
  },
  unshowOptions: {
    type: Array,
    default: () => []
  },
  default: {
    type: Boolean,
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: false
  }
})

const copyInvoiceType = ref()
const copyTaxRate = ref()
const copyChecked = ref()

watchEffect(() => {
  copyInvoiceType.value = props.invoiceType
  setDefault()
})

watchEffect(() => {
  copyTaxRate.value = props.taxRate
})

watchEffect(() => {
  copyChecked.value = props.checked
})

/**
 * 设置默认值
 * 有默认值的情况，并且value为空，则给value赋值
 */
function setDefault() {
  if (copyInvoiceType.value) {
    return
  }
  if (props.default) {
    copyInvoiceType.value = invoiceTypeEnum.SPECIAL.V
    handleInvoiceTypeChange(copyInvoiceType.value)
    return
  }
}

function checkChange(val) {
  emit('update:checked', val)
  if (val) {
    copyTaxRate.value = 0
    emit('update:taxRate', copyTaxRate.value)
  }
  emit('change', {})
}

function handleInvoiceTypeChange(val) {
  if (isBlank(val)) val = undefined
  copyChecked.value = false
  checkChange(copyChecked.value)
  if (val === invoiceTypeEnum.RECEIPT.V) {
    copyTaxRate.value = 0
    emit('update:taxRate', copyTaxRate.value)
  }
  emit('update:invoiceType', val)
  emit('change', {})
}

function handleTaxRateChange(val) {
  // var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,5}$/
  // if (!reg.test(val) || isBlank(val)) val = undefined
  if (isBlank(val)) val = undefined
  emit('update:taxRate', val)
  emit('change', {})
}

function selectBlur(e) {
  // if (!e.target.value) return
  // var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,5}$/
  // if (!reg.test(e.target.value.trim())) return
  copyTaxRate.value = e.target.value || copyTaxRate.value
  handleTaxRateChange(copyTaxRate.value)
  // emit('blur', e.target.value.trim())
}
</script>

<style lang="scss" scoped>
.invoice-type-select {
  display: inline-flex;
  > :nth-child(1) {
    flex: auto;
  }
  > :nth-child(2) {
    flex: none;
  }
}
</style>
