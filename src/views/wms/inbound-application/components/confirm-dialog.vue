<template>
  <common-dialog
    custom-class="inbound-application-preview"
    :title="`订单：${order.serialNumber}（${order.supplier ? order.supplier.name : ''}）`"
    append-to-body
    v-model="dialogVisible"
    width="1200px"
    :before-close="handleClose"
    :top="'5vh'"
    fullscreen
  >
    <template #titleAfter>
      <el-tag v-parse-enum="{ e: orderSupplyTypeEnum, v: order.supplyType }" type="danger" effect="plain" />
      <el-tag v-parse-enum="{ e: weightMeasurementModeEnum, v: order.weightMeasurementMode }" type="info" effect="plain" />
      <el-tag v-parse-enum="{ e: purchaseOrderPaymentModeEnum, v: order.purchaseOrderPaymentMode }" type="info" effect="plain" />
      <el-tag v-parse-enum="{ e: pickUpModeEnum, v: order.pickUpMode }" type="info" effect="plain" />
    </template>
    <common-table :data="form.list" :max-height="maxHeight" show-summary :summary-method="getSummaries">
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
      <material-base-info-columns :basic-class="props.basicClass" />
      <material-unit-quantity-columns :basic-class="props.basicClass" />
      <material-secondary-info-columns :basic-class="props.basicClass" />
    </common-table>
    <common-footer class="footer" unit="元" :total-value="amount" :show-total="showAmount" is-submit />
  </common-dialog>
</template>

<script setup>
import { computed, defineEmits, defineProps, ref } from 'vue'
import { orderSupplyTypeEnum, pickUpModeEnum, purchaseOrderPaymentModeEnum } from '@enum-ms/wms'
import { weightMeasurementModeEnum } from '@enum-ms/finance'
import { regExtra } from '@/composables/form/use-form'
import { cleanUpData } from '@/composables/form/use-table-validate'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import commonFooter from '../components/common-footer.vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

const emit = defineEmits(['saveSuccess', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: Number
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props })
const { cu, form, FORM } = regExtra() // 表单

// 价格填写时生效
const amount = ref() // 金额
const showAmount = ref(false) // 显示金额

const order = computed(() => {
  // 标题
  console.log('cu.props.order', cu.props.order)
  return cu.props.order || {}
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.class-measure-preview',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true
  },
  dialogVisible
)

// 表单提交数据清理
cu.submitFormFormat = async (form) => {
  cleanUpData(form.list)
  form.list = await numFmtByBasicClass(form.list, { toSmallest: true, isNum: true })

  return form
}

FORM.HOOK.afterSubmit = () => {
  handleClose()
}

function getSummaries(param) {
  return tableSummary(param, { props: ['number', 'mete'] })
}
</script>

<style lang="scss" scoped>
.inbound-application-preview {
  position: relative;
  .el-dialog__header .el-tag {
    min-width: 70px;
  }

  .footer {
    position: absolute;
    bottom: 0;
    left: 0;
  }
}
</style>
