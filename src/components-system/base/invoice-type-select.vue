<template>
  <span class="invoice-type-select child-mr-10">
    <common-select
      v-model="copyInvoiceType"
      :options="invoiceTypeEnum.ENUM"
      type="enum"
      clearable
      :disabled="props.disabled"
      style="width: 150px"
      placeholder="选择票据类型"
      @change="handleInvoiceTypeChange"
    />
    <template v-if="copyInvoiceType !== invoiceTypeEnum.RECEIPT.V">
      <common-select
        v-model="copyTaxRate"
        :options="taxRateOption"
        :disabled="props.disabled"
        :data-structure="{ key: 'id', label: 'label', value: 'value' }"
        allow-create
        style="width: 80px"
        clearable
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
// 未根据物料种类设置常用税率
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import EO from '@enum'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { supplierClassEnum } from '@enum-ms/supplier'
import { isBlank } from '@/utils/data-type'

import useTaxRate from '@compos/store/use-tax-rate'
import { uniqueArr } from '@/utils/data-type/array'

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
  },
  default: {
    type: Boolean,
    default: false
  }
})

const copyInvoiceType = ref()
const copyTaxRate = ref()
const { taxRateKV } = useTaxRate()

// 税率列表
const taxRateOption = computed(() => {
  const bitArr = EO.getBits(supplierClassEnum, props.classification, 'V')
  const res = []
  bitArr.forEach((bit) => {
    res.push.apply(res, taxRateKV.value[bit])
  })
  const opt = uniqueArr(res)
  if (opt[0]) handleTaxRateChange(opt[0])
  return opt.map((v) => {
    return {
      value: v,
      label: `${v}`
    }
  })
})

watch(
  () => props.invoiceType,
  (value) => {
    copyInvoiceType.value = value
    setDefault()
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

function handleInvoiceTypeChange(val) {
  if (isBlank(val)) val = undefined
  emit('update:invoiceType', val)
  emit('change', {})
}

function handleTaxRateChange(val) {
  var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,5}$/
  if (!reg.test(val) || isBlank(val)) val = undefined
  emit('update:taxRate', val)
  emit('change', {})
}

function selectBlur(e) {
  if (!e.target.value) return
  var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,5}$/
  if (!reg.test(e.target.value.trim())) return
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
