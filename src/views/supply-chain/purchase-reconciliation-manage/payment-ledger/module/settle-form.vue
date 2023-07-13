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
      <common-button v-loading="submitLoading" type="primary" size="mini" @click="onSubmit" v-if="showType==='add'">提交审核</common-button>
      <template v-else>
        <common-button v-loading="submitLoading" v-if="showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
        <common-button v-loading="submitLoading" v-if="showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
      </template>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px">
        <el-form-item label="采购合同编号">
          <span>{{ detailInfo.serialNumber }}</span>
        </el-form-item>
        <el-form-item label="供应商">
          <span>{{ detailInfo.supplierName }}</span>
        </el-form-item>
        <el-form-item label="合同额">
          <span v-thousand="detailInfo.amount" />
        </el-form-item>
        <el-form-item label="入库额">
          <span v-thousand="detailInfo.inboundAmount" />
        </el-form-item>
        <el-form-item label="已付款">
          <span v-thousand="detailInfo.paymentAmount"/>
          <span v-if="showType==='add'">（{{ detailInfo.paymentRate }}%）</span>
          <span v-else>（{{ detailInfo.inboundAmount? ((detailInfo.paymentAmount/detailInfo.inboundAmount)*100).toFixed(2)+'%': '0.00%'}}）</span>
        </el-form-item>
        <el-form-item label="已收票">
          <span v-thousand="detailInfo.invoiceAmount"/>
          <span v-if="showType==='add'">（{{ detailInfo.invoiceRate }}%）</span>
          <span v-else>（{{ detailInfo.inboundAmount? ((detailInfo.invoiceAmount/detailInfo.inboundAmount)*100).toFixed(2)+'%': '0.00%'}}）</span>
        </el-form-item>
        <el-form-item label="最终结算额"  prop="amount">
          <template v-if="showType==='add'">
            <el-input-number
              v-model="form.amount"
              :step="1"
              :min="detailInfo?.sourceRow?.paymentAmount?detailInfo?.sourceRow?.paymentAmount:0"
              :max="999999999999"
              :precision="decimalPrecision.supplyChain"
              placeholder="最终结算额"
              controls-position="right"
              style="width: 220px"
            />
          </template>
          <template v-else>
            <span v-if="showType==='audit'">{{ detailInfo.unCheckSettlementAmount?toThousand(detailInfo.unCheckSettlementAmount,decimalPrecision.supplyChain):'-' }}</span>
            <span v-else>{{ detailInfo.settlementAmount?toThousand(detailInfo.settlementAmount,decimalPrecision.supplyChain):'-' }}</span>
          </template>
        </el-form-item>
        <el-form-item label="大写">
          <span style="color:#82848a"  v-if="showType==='add'">{{ form.amount?digitUppercase(form.amount):'' }}</span>
           <template v-else>
            <span v-if="showType==='audit'">{{ detailInfo.unCheckSettlementAmount?digitUppercase(detailInfo.unCheckSettlementAmount,decimalPrecision.supplyChain):'-' }}</span>
            <span style="color:#82848a"  v-else>{{ detailInfo.settlementAmount?digitUppercase( detailInfo.settlementAmount,decimalPrecision.supplyChain):'' }}</span>
          </template>
        </el-form-item>
        <el-form-item label="应付金额">
          <span v-if="showType==='audit'">{{ toThousand((detailInfo.unCheckSettlementAmount-detailInfo.paymentAmount),decimalPrecision.supplyChain) }}</span>
          <span v-else-if="showType==='add'">{{ form.amount?toThousand((form.amount-detailInfo?.sourceRow?.paymentAmount),decimalPrecision.supplyChain):'' }}</span>
          <span v-else>{{ toThousand((detailInfo.settlementAmount-detailInfo.paymentAmount),decimalPrecision.supplyChain) }}</span>
        </el-form-item>
        <el-form-item label="应补发票">
          <span v-if="showType==='audit'">{{ toThousand((detailInfo.unCheckSettlementAmount-detailInfo.invoiceAmount),decimalPrecision.supplyChain) }}</span>
          <span v-else-if="showType==='add'">{{ form.amount?toThousand((form.amount-detailInfo?.sourceRow?.invoiceAmount),decimalPrecision.supplyChain):'' }}</span>
          <span v-else>{{ toThousand((detailInfo.settlementAmount-detailInfo.invoiceAmount),decimalPrecision.supplyChain) }}</span>
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-if="showType==='add'"
            v-model="form.remark"
            type="textarea"
            style="width: 100%"
            maxlength="200"
            show-word-limit
            :autosize="{ minRows: 2, maxRows: 4 }"
            placeholder="请输入备注"
          />
          <span v-else>{{ detailInfo.settlementRemark }}</span>
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
          <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.jpg,.png,.pdf,.jpeg'" v-if="showType==='add'"/>
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
import { settleSave, settleConfirm } from '@/api/supply-chain/purchase-reconciliation-manage/payment-application'
import { ref, defineProps, watch, defineEmits, nextTick } from 'vue'
import useVisible from '@compos/use-visible'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { fileClassifyEnum } from '@enum-ms/file'
import { auditTypeEnum } from '@enum-ms/contract'
import { digitUppercase, toThousand } from '@data-type/number'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { ElNotification } from 'element-plus'
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
  amount: undefined,
  attachmentIds: undefined,
  attachments: [],
  orderId: undefined,
  propertyType: undefined,
  remark: ''
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const uploadRef = ref()
const pdfShow = ref(false)
const currentId = ref()
const validateMoney = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请填写申请金额并大于0'))
  }
  callback()
}

const rules = {
  amount: [{ required: true, validator: validateMoney, trigger: 'blur' }]
}

watch(
  () => visible.value,
  (val) => {
    if (val) {
      if (!props.showType) {
        resetForm()
      }
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
  if (data && Object.keys(data).length > 0) {
    form.value = data
  } else {
    form.value = JSON.parse(JSON.stringify(defaultForm))
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
    form.value.orderId = props.detailInfo.id
    form.value.propertyType = props.detailInfo.propertyType
    form.value.attachmentIds = form.value.attachments ? form.value.attachments.map((v) => v.id) : undefined
    await settleSave(form.value)
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
      orderId: props.detailInfo.id
    }
    await settleConfirm(submitForm)
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
