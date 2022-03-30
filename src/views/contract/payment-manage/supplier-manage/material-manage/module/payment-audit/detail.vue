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
      <el-descriptions-item label-class-name="contractLabel" label="申请人">{{currentInfo.applyUserName}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="申请日期">{{currentInfo.createTime? parseTime(currentInfo.createTime,'{y}-{m}-{d}'): '-'}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="采购单号">{{currentInfo.serialNumber}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="所属项目">
        <template v-if="currentInfo.projects && currentInfo.projects.length>0">
          <span v-for="item in currentInfo.projects" :key="item.id">
            {{item.serialNumber+' '+item.shortName}}
          </span>
        </template>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="供应商">{{currentInfo.supplierName}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="合同额">{{currentInfo.amount}}</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="已付款">
        <span>{{ currentInfo.paymentAmount?toThousand(currentInfo.paymentAmount):'-' }}</span>
        <span style="margin-left:5px;" v-if="currentInfo.amount">{{ (currentInfo.paymentAmount/currentInfo.amount)*100+'%' }}</span>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="最近一次付款">
        <span>{{ currentInfo.lastPaymentAmount?toThousand(currentInfo.lastPaymentAmount):'-' }}</span>
        <span v-if="currentInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(currentInfo.lastPaymentTime,'{y}-{m}-{d}')}}</span>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="本次申请">
        <el-tag effect="plain">{{currentInfo.applyAmount?toThousand(currentInfo.applyAmount):'-'}}</el-tag>
        <el-tag style="margin-left:5px;" v-if="currentInfo.applyAmount && currentInfo.amount">{{ (currentInfo.applyAmount/currentInfo.amount)*100+'%' }}</el-tag>
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
        <el-tag type="success" style="margin-left:5px;" v-if="actuallyPaymentAmount && currentInfo.amount">{{ (actuallyPaymentAmount/currentInfo.amount)*100+'%' }}</el-tag>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="付款方式">
        <common-select
          v-model="paymentMethod"
          :options="paymentFineModeEnum.ENUM"
          type="enum"
          size="small"
          placeholder="付款方式"
          style="width:100%;"
        />
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="付款银行">
        <common-select
          v-model="paymentBankAccount"
          :options="bankList"
          type="other"
          :dataStructure="typeProp"
          size="small"
          placeholder="收款银行"
          style="width:100%;"
          @change="bankChange"
        />
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="附件">
        <template v-if="currentInfo && currentInfo.attachments && currentInfo.attachments.length>0">
          <div v-for="item in currentInfo.attachments" :key="item.id">{{item.name}}
            <export-button :params="{id: item.id}"/>
          </div>
        </template></el-descriptions-item>
    </el-descriptions>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, watch, defineEmits } from 'vue'
import { auditTypeEnum } from '@enum-ms/contract'
import useVisible from '@compos/use-visible'
import { payDetail, editStatus } from '@/api/contract/supplier-manage/pay-invoice/pay'
import { bankData } from '@/api/contract/collection-and-invoice/collection'
import { digitUppercase, toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { paymentFineModeEnum } from '@enum-ms/finance'
import useDict from '@compos/store/use-dict'
import { ElNotification, ElMessage } from 'element-plus'
import ExportButton from '@comp-common/export-button/index.vue'

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
  }
})

const dict = useDict(['payment_reason'])
const bankList = ref([])
const typeProp = { key: 'id', label: 'depositBank', value: 'id' }
const actuallyPaymentAmount = ref()
const paymentBankAccount = ref()
const paymentBank = ref()
const paymentMethod = ref()
const currentInfo = ref({})
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
</style>
