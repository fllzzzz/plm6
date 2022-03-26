<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    :title="`订单结算(${detailInfo.serialNumber})`"
    :wrapper-closable="false"
    size="600px"
  >
    <template #titleRight>
      <common-button v-loading="submitLoading" type="primary" size="mini" @click="onSubmit">提交审核</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="150px">
        <el-form-item label="项目">
          <template v-if="detailInfo.projects && detailInfo.projects.length>0">
            <div v-for="item in detailInfo.projects" :key="item.id">
              {{item.serialNumber+' '+item.shortName}}
            </div>
          </template>
        </el-form-item>
        <el-form-item label="供应商">
          <span>{{ detailInfo.supplierName }}</span>
        </el-form-item>
        <el-form-item label="合同额(元)">
          <span>{{ detailInfo.amount?toThousand(detailInfo.amount):'-' }}</span>
        </el-form-item>
        <el-form-item label="已付款(元)">
          <span>{{ detailInfo.paymentAmount }}</span>
          <el-tag style="margin-left:5px;" v-if="detailInfo.amount">{{ (detailInfo.paymentAmount/detailInfo.amount)*100+'%' }}</el-tag>
        </el-form-item>
        <el-form-item label="最终结算额(元)"  prop="amount">
          <el-input-number
              v-show-thousand
              v-model="form.amount"
              :step="1"
              :min="detailInfo.paymentAmount?detailInfo.paymentAmount:0"
              :max="detailInfo.amount?detailInfo.amount:999999999999"
              :precision="DP.YUAN"
              controls-position="right"
              style="width: 220px"
            />
            <span style="color:#82848a">{{form.amount?digitUppercase(form.amount):''}}</span>
        </el-form-item>
        <el-form-item label="应付(元)">
          <span v-if="form.amount">{{ toThousand(form.amount-detailInfo.paymentAmount) }}</span>
        </el-form-item>
        <el-form-item label="应补发票(元)">
          <span v-if="form.amount">{{ toThousand(form.amount-detailInfo.invoiceAmount) }}</span>
        </el-form-item>
        <el-form-item label="附件">
          <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.zip,.jpg,.png,.pdf,.jpeg'"/>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { settleSave } from '@/api/supply-chain/purchase-reconciliation-manage/payment-application'
import { ref, defineProps, watch, defineEmits, nextTick } from 'vue'
import useVisible from '@compos/use-visible'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { DP } from '@/settings/config'
import { fileClassifyEnum } from '@enum-ms/file'
import { digitUppercase, toThousand } from '@data-type/number'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { ElNotification } from 'element-plus'

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
  }
})

const defaultForm = {
  amount: undefined,
  attachmentIds: undefined,
  attachments: undefined,
  orderId: undefined,
  propertyType: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const uploadRef = ref()
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
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

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
  try {
    const valid = await formRef.value.validate()
    if (!valid) {
      return
    }
    form.value.orderId = props.detailInfo.id
    form.value.propertyType = props.detailInfo.propertyType
    form.value.attachmentIds = form.value.attachments ? form.value.attachments.map((v) => v.id) : undefined
    await settleSave(form.value)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('项目结算失败', error)
  }
}

</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
