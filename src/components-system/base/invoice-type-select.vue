<template>
  <span>
    <common-select
      v-model="copyInvoiceType"
      :options="invoiceTypeEnum.ENUM"
      type="enum"
      clearable
      :disabled="props.disabled"
      style="width: 150px"
      placeholder="请选择票据类型"
      @change="handleInvoiceTypeChange"
    />
    <common-select
      v-if="form.invoiceType !== invoiceTypeEnum.RECEIPT.V"
      v-model="copyTaxRate"
      :options="taxRateOption"
      :disabled="props.disabled"
      allow-create
      style="width: 100px"
      clearable
      @blur="taxBlur"
      @change="handleTaxRateChange"
    />
  </span>
</template>

<script setup>
// 未根据物料种类设置常用税率
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { isBlank } from '@/utils/data-type'

import useTaxRate from '@compos/store/use-tax-rate'

const emit = defineEmits(['update:invoiceType', 'update:taxRate', 'change'])

const props = defineProps({
  invoiceType: {
    type: Number
  },
  taxRate: {
    type: Number
  },
  classification: {
    type: Number
  }
})

const copyInvoiceType = ref()
const copyTaxRate = ref()
const { taxRateKV } = useTaxRate()

// 税率列表
const taxRateOption = computed(() => {
  const opt = taxRateKV.value[props.classification] || []
  return opt.map(v => {
    return {
      value: v,
      label: v
    }
  })
})

watch(
  () => props.invoiceType,
  (value) => {
    copyInvoiceType.value = value
  },
  { immediate: true }
)

watch(
  () => props.taxRate,
  (value) => {
    copyTaxRate.value = value
  },
  { immediate: true }
)

function handleInvoiceTypeChange(val) {
  if (isBlank(val)) val = undefined
  emit('update:invoiceType', val)
  emit('change', {})
}

function handleTaxRateChange(val) {
  if (isBlank(val)) val = undefined
  emit('update:taxRate', val)
  emit('change', {})
}

// function selectBlur(e) {
//   if (!e.target.value) return
//   var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,5}$/
//   if (!reg.test(e.target.value.trim())) return
//   copyTaxRate.value = e.target.value || copyTaxRate.value
//   emit('blur', e.target.value.trim())
// }

</script>
