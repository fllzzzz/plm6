<template>
   <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="付款申请详情"
    :wrapper-closable="false"
    size="40%"
  >
    <template #titleAfter>
      <el-tag v-if="currentRow.auditStatus" size="medium" :type="currentRow.auditStatus===auditTypeEnum.REJECT.V?'info':(currentRow.auditStatus===auditTypeEnum.PASS.V?'success':'warning')">
        {{ currentRow.auditStatus===auditTypeEnum.REJECT.V?'已驳回':(currentRow.auditStatus===auditTypeEnum.PASS.V?'已通过':'审核中') }}
      </el-tag>
    </template>
    <template #content>
      <el-form ref="formRef" size="small" label-width="130px">
        <el-form-item label="申请日期" prop="paymentDate">
          <span v-if="currentRow.paymentDate" style="margin-left:5px;">{{ parseTime(currentRow.paymentDate,'{y}-{m}-{d}')}}</span>
        </el-form-item>
         <el-form-item label="所属项目">
          <span>{{ props.detailInfo?.project }}</span>
        </el-form-item>
        <el-form-item label="分包公司">
          <span>{{ detailInfo?.supplierName }}</span>
        </el-form-item>
         <el-form-item label="合同额">
          <span v-thousand="detailInfo?.amount" />
        </el-form-item>
        <el-form-item label="已付款">
          <span v-thousand="detailInfo?.paymentAmount"/><span>（{{ detailInfo?.paymentRate }}%）</span>
        </el-form-item>
        <el-form-item label="最近一次付款">
          <span  v-thousand="detailInfo?.lastPaymentAmount" v-empty-text/>
          <el-tag v-if="detailInfo?.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo?.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
        </el-form-item>
        <el-form-item label="结算额" v-if="detailInfo?.sourceRow?.settlementAmount">
          <span v-thousand="detailInfo?.settlementAmount" />
        </el-form-item>
        <el-form-item label="本次项目违法处罚" prop="notBoolDeduct">
          <span v-thousand="currentRow.breachAmount"  v-empty-text />
          <el-checkbox v-model="notBoolDeduct" label="本次不扣除" size="small" style="margin-left:20px;vertical-align:middle;" disabled/>
        </el-form-item>
        <el-form-item label="本次付款" prop="inputAmount">
          <span v-thousand="inputAmount" />
        </el-form-item>
        <el-form-item label="大写">
          <span style="color:#82848a">{{inputAmount?digitUppercase(inputAmount):''}}</span>
        </el-form-item>
         <el-form-item label="收款银行">
          <span>{{detailInfo?.supplier.bankName}}</span>
        </el-form-item>
        <el-form-item label="账号">
          <span>{{detailInfo?.supplier.bankAccount}}</span>
        </el-form-item>
        <el-form-item label="申请金额">
          <span v-thousand="currentRow?.applyAmount" />
        </el-form-item>
        <el-form-item label="付款事由" prop="paymentReasonId">
          <div>{{ currentRow.paymentReasonId && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ currentRow.paymentReasonId]: '' }}</div>
        </el-form-item>
        <el-form-item label="附件">
          <template #label>
            附件
            <el-tooltip
              effect="light"
              :content="`双击可预览附件`"
              placement="top"
              v-if="currentRow.attachments?.length && !currentRow.files?.length"
            >
              <i class="el-icon-info" />
            </el-tooltip>
          </template>
          <template v-if="props.currentRow.attachments?.length && !currentRow.files?.length">
            <div v-for="item in currentRow.attachments" :key="item.id">
              <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
            </div>
          </template>
        </el-form-item>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, defineEmits, computed } from 'vue'

import { digitUppercase } from '@data-type/number'
import { parseTime } from '@/utils/date'
import useVisible from '@compos/use-visible'
import useDict from '@compos/store/use-dict'
import { auditTypeEnum } from '@enum-ms/contract'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const formRef = ref()
const dict = useDict(['payment_reason'])
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const pdfShow = ref(false)
const currentId = ref()

const inputAmount = computed(() => {
  return props.currentRow.applyAmount + props.currentRow.breachAmount
})

const notBoolDeduct = computed(() => {
  return !props.currentRow.boolDeduct
})

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
