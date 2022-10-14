<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="60%"
    :before-close="handleClose"
    title="审核详情"
    :center="false"
    :close-on-click-modal="false"
  >
    <template #title>
      <div class="dialog-title">
        <span class="title-left">审核详情</span>
        <el-tag v-if="detailInfo.auditStatus" size="medium" :type="detailInfo.auditStatus==auditTypeEnum.REJECT.V?'info':(detailInfo.auditStatus==auditTypeEnum.PASS.V?'success':'warning')">
          {{ detailInfo.auditStatus==auditTypeEnum.REJECT.V?'已驳回':(detailInfo.auditStatus==auditTypeEnum.PASS.V?'已通过':'审核中') }}
        </el-tag>
        <span style="position:absolute;right:20px;">
          <common-button v-if="showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
          <common-button v-if="showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
          <common-button size="small"  @click="handleClose">关闭</common-button>
        </span>
      </div>
    </template>
    <el-descriptions class="margin-top" :column="2" border>
      <el-descriptions-item label-class-name="contractLabel" label="申请人" >{{currentInfo.applyUserName}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="申请日期">{{currentInfo.createTime? parseTime(currentInfo.createTime,'{y}-{m}-{d}'): '-'}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="采购合同编号">{{currentRow.serialNumber}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="所属项目">
         <span class="project-name">{{ projectNameFormatter(currentRow.project) }}</span>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="供应商">{{currentRow.supplierName}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="合同额">
        {{currentRow.amount?toThousand(currentRow.amount):'-'}}
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="已付款">
        <span>{{ currentRow.paymentAmount?toThousand(currentRow.paymentAmount):'-' }}</span>
        <span style="margin-left:5px;" v-if="currentRow.paymentAmount && currentRow.inboundAmount">{{ ((currentRow.paymentAmount/currentRow.inboundAmount)*100).toFixed(2)+'%' }}</span>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="最近一次付款">
        <span>{{ currentRow.lastPaymentAmount?toThousand(currentRow.lastPaymentAmount):'-' }}</span>
        <span v-if="currentRow.lastPaymentTime" style="margin-left:5px;">{{ parseTime(currentRow.lastPaymentTime,'{y}-{m}-{d}')}}</span>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="本次申请">
        <el-tag effect="plain">{{currentInfo.applyAmount?toThousand(currentInfo.applyAmount):'-'}}</el-tag>
        <el-tag style="margin-left:5px;" v-if="currentInfo.applyAmount && currentRow.inboundAmount">{{ ((currentInfo.applyAmount/currentRow.inboundAmount)*100).toFixed(2)+'%' }}</el-tag>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="大写">
        <div>{{currentInfo.applyAmount?'('+digitUppercase(currentInfo.applyAmount)+')':'-'}}</div>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="付款事由">
        {{ currentInfo.paymentReasonId && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][currentInfo.paymentReasonId]: '' }}
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="收款单位">{{currentInfo.receivingUnit}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="收款银行">{{currentInfo.receiveBank}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="账号">{{currentInfo.receiveBankAccount}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="本次实付">
        <el-input-number
          v-if="showType==='audit'"
          v-model.number="actuallyPaymentAmount"
          v-show-thousand
          :min="0"
          :max="detailInfo.applyAmount"
          :step="100"
          :precision="DP.YUAN"
          placeholder="本次实付(元)"
          controls-position="right"
          style="width:220px;"
        />
        <span v-else>{{currentInfo.actuallyPaymentAmount?toThousand(currentInfo.actuallyPaymentAmount):'-'}}</span>
        <template v-if="showType==='audit'">
          <el-tag type="success" style="margin-left:5px;" v-if="actuallyPaymentAmount && currentRow.inboundAmount">{{ ((actuallyPaymentAmount/currentRow.inboundAmount)*100).toFixed(2)+'%' }}</el-tag>
        </template>
        <template v-else>
          <el-tag type="success" style="margin-left:5px;" v-if="currentInfo.actuallyPaymentAmount && currentRow.inboundAmount">{{ ((currentInfo.actuallyPaymentAmount/currentRow.inboundAmount)*100).toFixed(2)+'%' }}</el-tag>
        </template>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="付款方式">
        <common-select
          v-if="showType==='audit'"
          v-model="paymentMethod"
          :options="paymentFineModeEnum.ENUM"
          type="enum"
          size="small"
          placeholder="付款方式"
          style="width:100%;"
        />
        <span v-else>{{ currentInfo.paymentMethod?paymentFineModeEnum.VL[currentInfo.paymentMethod]:'-' }}</span>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="付款银行">
        <common-select
          v-if="showType==='audit'"
          v-model="paymentBankAccount"
          :options="bankList"
          type="other"
          :dataStructure="typeProp"
          size="small"
          placeholder="收款银行"
          style="width:100%;"
          @change="bankChange"
        />
        <span v-else>{{ currentInfo.paymentBank?currentInfo.paymentBank:'-' }}</span>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="附件">
        <template v-if="currentInfo && currentInfo.attachments && currentInfo.attachments.length>0">
          <div v-for="item in currentInfo.attachments" :key="item.id">
            <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
          </div>
        </template>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="remark" label="备注" :span="2">
        <span>{{currentInfo.remark}}</span>
      </el-descriptions-item>
    </el-descriptions>
    <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, watch, defineEmits } from 'vue'
import { auditTypeEnum } from '@enum-ms/contract'

import { projectNameFormatter } from '@/utils/project'
import useVisible from '@compos/use-visible'
import { payDetail } from '@/api/contract/supplier-manage/pay-invoice/pay'
import { editStatus } from '@/api/project-manage/subcontract-payment/payment-application'
import { bankData } from '@/api/contract/collection-and-invoice/collection'
import { digitUppercase, toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { paymentFineModeEnum } from '@enum-ms/finance'
import useDict from '@compos/store/use-dict'
import { ElNotification, ElMessage } from 'element-plus'

import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: undefined
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  branchCompanyId: {
    type: [String, Number],
    default: undefined
  },
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const dict = useDict(['payment_reason'])
const bankList = ref([])
const typeProp = { key: 'account', label: 'depositBank', value: 'account' }
const actuallyPaymentAmount = ref()
const paymentBankAccount = ref()
const paymentBank = ref()
const paymentMethod = ref()
const currentInfo = ref({})
const pdfShow = ref(false)
const currentId = ref()
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      currentInfo.value = {}
      actuallyPaymentAmount.value = undefined
      paymentBank.value = undefined
      paymentBankAccount.value = undefined
      paymentMethod.value = undefined
      getDetail(props.detailInfo.id)
      getBankData(props.branchCompanyId)
    }
  },
  { deep: true, immediate: true }
)

async function getDetail(paymentId) {
  try {
    const data = await payDetail(paymentId)
    currentInfo.value = data || {}
    actuallyPaymentAmount.value = currentInfo.value.applyAmount || 0
  } catch (e) {
    console.log('获取付款详情', e)
  }
}

async function getBankData(companyId) {
  try {
    const { content } = await bankData(companyId)
    bankList.value = content
  } catch (e) {
    console.log('获取银行账号', e)
  }
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

function bankChange(val) {
  if (val) {
    const choseVal = bankList.value.find(v => v.account === val)
    paymentBank.value = choseVal.depositBank
  } else {
    paymentBank.value = undefined
  }
}

async function passConfirm(val) {
  if (val === auditTypeEnum.PASS.V) {
    if (!actuallyPaymentAmount.value) {
      ElMessage.error('本次实付必填且大于0')
      return
    }
    if (!paymentMethod.value) {
      ElMessage.error('付款方式必填')
      return
    }
    if (!paymentBankAccount.value) {
      ElMessage.error('付款银行必填')
      return
    }
  }
  try {
    const submitData = {
      auditStatus: val,
      actuallyPaymentAmount: actuallyPaymentAmount.value,
      id: currentInfo.value.id,
      paymentBank: paymentBank.value,
      paymentBankAccount: paymentBankAccount.value,
      paymentMethod: paymentMethod.value
    }
    await editStatus(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
  .title-left {
    display: flex;
    align-items: center;
    position: relative;
    padding-left: 10px;
    margin-right: 15px;
    box-sizing: border-box;
  }
  .title-left::before {
    content: "";
    width: 4px;
    height: 15px;
    border-radius: 10px;
    background: #1890ff;
    position: absolute;
    top: 50%;
    left: 0;
    transform: translateY(-50%);
}
.tip-red{
  color:red;
}
.tip-green{
  color:#67c23a;
}
.detail-break{
  word-break:break-all;
}
::v-deep(.contractLabel.el-descriptions__label){
   min-width:150px;
}
</style>
