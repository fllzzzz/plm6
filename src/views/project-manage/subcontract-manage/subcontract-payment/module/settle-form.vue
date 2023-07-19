<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="订单结算"
    :wrapper-closable="false"
    size="40%"
  >
    <template #titleAfter>
      <el-tag v-if="showType==='detail'">
        已结算
      </el-tag>
    </template>
    <template #titleRight>
      <common-button v-loading="submitLoading" type="primary" size="mini" @click="onSubmit" v-if="showType==='add' || showType==='edit'">提交审核</common-button>
      <template v-if="showType==='audit'">
        <common-button v-loading="submitLoading" v-if="showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
        <common-button v-loading="submitLoading" v-if="showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
      </template>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px">
        <el-form-item label="申请日期" prop="applyDate">
          <el-date-picker
            v-if="showType==='add' || showType==='edit'"
            v-model="form.applyDate"
            type="date"
            value-format="x"
            placeholder="选择申请日期"
            :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
            style="width: 220px"
          />
          <span v-else>{{ detailInfo?.lastPaymentTime?parseTime(detailInfo?.lastPaymentTime,'{y}-{m}-{d}'):'-'}}</span>
        </el-form-item>
        <el-form-item label="所属项目">
          <span>{{ detailInfo.serialNumber }}</span>
        </el-form-item>
        <el-form-item label="分包单位">
          <span>{{ detailInfo.supplierName }}</span>
        </el-form-item>
        <el-form-item label="合同额">
          <span  v-thousand="detailInfo.amount" />
        </el-form-item>
         <el-form-item label="已付款">
          <span v-thousand="detailInfo.paymentAmount"/>
          <span>（{{ detailInfo.paymentRate }}%）</span>
        </el-form-item>
        <el-form-item label="最近一次付款">
          <span  v-thousand="detailInfo?.lastPaymentAmount" />
          <el-tag v-if="detailInfo?.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo?.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
        </el-form-item>
        <el-form-item label="签证额">
          <span  v-thousand="detailInfo.visaAmount" />
        </el-form-item>
        <el-form-item label="已收票">
          <span v-thousand="detailInfo.invoiceAmount"/>
          <span>（{{ detailInfo.invoiceRate }}%）</span>
        </el-form-item>
        <el-form-item label="项目违法处罚">
          <span v-thousand="breachAmount"/>
          <el-checkbox v-model="form.notBoolDeduct" label="不扣除" size="small" style="margin-left:30px;vertical-align:middle;" :disabled="!breachAmount" v-if="showType==='add' || showType==='edit'"/>
          <el-checkbox v-model="form.notBoolDeduct" label="不扣除" size="small" style="margin-left:30px;vertical-align:middle;" disabled v-else/>
        </el-form-item>
        <el-form-item label="最终结算额"  prop="amount">
          <template v-if="showType==='add' || showType==='edit'">
            <el-input-number
              v-model="form.amount"
              :step="1"
              :min="detailInfo?.sourceRow?.paymentAmount?detailInfo?.sourceRow?.paymentAmount:0"
              :max="999999999999"
              :precision="decimalPrecision.project"
              placeholder="最终结算额"
              controls-position="right"
              style="width: 220px"
            />
          </template>
          <template v-else>
            <span v-if="showType==='audit'">{{ detailInfo.unCheckSettlementAmount?toThousand((detailInfo.unCheckSettlementAmount),decimalPrecision.project):'-' }}</span>
            <span v-else>{{ detailInfo.settlementAmount?toThousand((detailInfo.settlementAmount),decimalPrecision.project):'-' }}</span>
          </template>
        </el-form-item>
        <el-form-item label="大写">
          <span style="color:#82848a"  v-if="showType==='add' || showType==='edit'">{{ form.amount?digitUppercase(form.amount):'' }}</span>
           <template v-else>
            <span v-if="showType==='audit'">{{ detailInfo.unCheckSettlementAmount?digitUppercase(detailInfo.unCheckSettlementAmount):'-' }}</span>
            <span style="color:#82848a"  v-else>{{ detailInfo.settlementAmount?digitUppercase( detailInfo.settlementAmount):'' }}</span>
          </template>
        </el-form-item>
        <el-form-item label="应付金额">
          <span v-if="showType==='audit'">{{ toThousand((detailInfo.unCheckSettlementAmount-detailInfo.paymentAmount),decimalPrecision.project) }}</span>
          <span v-else-if="showType==='add' || showType==='edit'">{{ form.amount?toThousand((form.amount-detailInfo?.sourceRow?.paymentAmount),decimalPrecision.project):'' }}</span>
          <span v-else>{{ toThousand((detailInfo.settlementAmount-detailInfo.paymentAmount),decimalPrecision.project) }}</span>
        </el-form-item>
        <el-form-item label="应补发票">
          <span v-if="showType==='audit'">{{ toThousand((detailInfo.unCheckSettlementAmount-detailInfo.invoiceAmount),decimalPrecision.project) }}</span>
          <span v-else-if="showType==='add' || showType==='edit'">{{ form.amount?toThousand((form.amount-detailInfo?.sourceRow?.invoiceAmount),decimalPrecision.project):'' }}</span>
          <span v-else>{{ toThousand((detailInfo.settlementAmount-detailInfo.invoiceAmount),decimalPrecision.project) }}</span>
        </el-form-item>
        <el-form-item label="附件">
          <template #label>
            附件
            <el-tooltip
              effect="light"
              :content="`双击可预览附件`"
              placement="top"
              v-if="detailInfo.attachmentDTOS && detailInfo.attachmentDTOS.length>0"
            >
              <i class="el-icon-info" />
            </el-tooltip>
          </template>
          <template v-if="showType==='add' || showType==='edit'">
            <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.jpg,.png,.pdf,.jpeg'" />
            <div v-if="form.attachmentDTOS && form.attachmentDTOS.length>0 && !form.attachments.length">
              <div v-for="item in form.attachmentDTOS" :key="item.id">
                <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
              </div>
            </div>
          </template>
          <template v-else>
            <div v-if="detailInfo.attachmentDTOS && detailInfo.attachmentDTOS.length>0">
              <div v-for="item in detailInfo.attachmentDTOS" :key="item.id">
                <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
              </div>
            </div>
          </template>
        </el-form-item>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { settleSave, check, settleEdit } from '@/api/project-manage/subcontract-payment/payment-application'
import { ref, defineProps, watch, defineEmits, nextTick, computed } from 'vue'

import { isNotBlank } from '@data-type/index'
import useVisible from '@compos/use-visible'
import { fileClassifyEnum } from '@enum-ms/file'
import { auditTypeEnum } from '@enum-ms/contract'
import { digitUppercase, toThousand } from '@data-type/number'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { ElNotification } from 'element-plus'
import { parseTime } from '@/utils/date'

import UploadBtn from '@comp/file-upload/UploadBtn'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const submitLoading = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: undefined
  }
})

const defaultForm = {
  applyDate: undefined,
  amount: undefined,
  notBoolDeduct: undefined, // 不扣除
  boolDeduct: undefined,
  attachmentIds: undefined,
  attachments: [],
  orderId: undefined,
  remark: ''
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const uploadRef = ref()
const pdfShow = ref(false)
const currentId = ref()
const breachAmount = computed(() => {
  return props.detailInfo.penalty
})
const validateMoney = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请填写申请金额并大于0'))
  }
  callback()
}

const rules = {
  applyDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }],
  amount: [{ required: true, validator: validateMoney, trigger: 'blur' }]
}

watch(
  () => visible.value,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

function resetForm(data) {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value = JSON.parse(JSON.stringify(defaultForm))
  if (props.showType === 'edit') {
    form.value.applyDate = props.detailInfo.settlementInfo?.applyDate
    form.value.amount = props.detailInfo.settlementInfo?.amount
    form.value.boolDeduct = props.detailInfo.settlementInfo?.boolDeduct
    form.value.attachmentDTOS = props.detailInfo.settlementInfo?.attachments
    form.value.orderId = props.detailInfo.id
    form.value.remark = props.detailInfo.settlementInfo?.remark
    form.value.notBoolDeduct = !form.value.boolDeduct
  }

  if (props.showType !== 'add' && props.showType !== 'edit') {
    form.value.notBoolDeduct = props.detailInfo.settleNotBoolDeduct
  }
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

async function onSubmit() {
  submitLoading.value = true
  try {
    const valid = await formRef.value.validate()
    if (!valid) {
      return
    }
    form.value.boolDeduct = isNotBlank(form.value.notBoolDeduct) ? !form.value.notBoolDeduct : (breachAmount.value ? undefined : false)
    form.value.orderId = props.detailInfo.id
    form.value.attachmentIds = form.value.attachments.length ? form.value.attachments.map((v) => v.id) : (props.detailInfo.settlementInfo?.attachments.length ? props.detailInfo.settlementInfo?.attachments.map((v) => v.id) : undefined)
    if (props.showType === 'add') {
      await settleSave(form.value)
    } else {
      await settleEdit(form.value)
    }
    submitLoading.value = false
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    submitLoading.value = false
    console.log('项目结算失败', error)
  }
}

async function passConfirm(val) {
  submitLoading.value = true
  const msg = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
  try {
    const submitForm = {
      auditStatus: val,
      id: props.detailInfo?.settlementInfo?.id
    }
    await check(submitForm)
    submitLoading.value = false
    ElNotification({ title: '审核' + msg, type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    submitLoading.value = false
    console.log('项目结算审核失败', error)
  }
}

</script>
