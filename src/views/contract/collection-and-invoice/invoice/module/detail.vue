<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="closeDrawer"
    title="开票信息"
    :wrapper-closable="false"
    size="860px"
  >
    <template #title>
      <div class="dialog-title">
        <span style="margin-right: 5px">开票信息</span>
        <common-button
          v-if="collectionInfo.auditStatus"
          size="mini"
          :type="
            collectionInfo.auditStatus == auditTypeEnum.ENUM.REJECT.V
              ? 'info'
              : collectionInfo.auditStatus == auditTypeEnum.ENUM.PASS.V
              ? 'success'
              : 'warning'
          "
        >
          {{
            collectionInfo.auditStatus == auditTypeEnum.ENUM.REJECT.V
              ? '已驳回'
              : collectionInfo.auditStatus == auditTypeEnum.ENUM.PASS.V
              ? '已通过'
              : '审核中'
          }}
        </common-button>
        <span style="position: absolute; right: 20px">
          <template v-if="collectionInfo.auditStatus">
            <template v-if="!isModify">
              <common-button
                v-if="collectionInfo.auditStatus == auditTypeEnum.ENUM.AUDITING.V && type === 'audit'"
                size="small"
                type="info"
                @click="onSubmit(auditTypeEnum.ENUM.REJECT.V)"
                >驳回</common-button
              >
              <common-button
                v-if="collectionInfo.auditStatus == auditTypeEnum.ENUM.AUDITING.V && type === 'audit'"
                size="small"
                type="success"
                @click="onSubmit(auditTypeEnum.ENUM.PASS.V)"
                >通过</common-button
              >
              <common-button
                size="small"
                type="primary"
                @click="modifyInfo"
                v-if="collectionInfo.auditStatus == auditTypeEnum.ENUM.REJECT.V && type === 'detail'"
                >重新编辑</common-button
              >
            </template>
            <template v-else>
              <common-button type="primary" size="small" @click="onSubmit">提交</common-button>
            </template>
          </template>
          <common-button size="small" @click="closeDrawer">关闭</common-button>
        </span>
      </div>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <div class="form-row" style="display: flex">
          <el-form-item label="项目" prop="projectId">
            <div style="width: 260px">
              <project-cascader
                v-if="isModify"
                v-model="form.projectId"
                style="width: 250px"
                class="filter-item"
                @change="getContractInfo(form.projectId)"
              />
              <span v-else>{{ collectionInfo.project.serialNumber + ' ' + collectionInfo.project.shortName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="发票类型" prop="invoiceType">
            <div style="width: 260px">
              <common-select
                v-if="isModify"
                v-model="form.invoiceType"
                :options="invoiceTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="发票类型"
                style="width: 250px"
              />
              <template v-else>
                <span>{{ collectionInfo.invoiceType ? invoiceTypeEnum.VL[collectionInfo.invoiceType] : '' }}</span>
                <span v-if="collectionInfo.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V">{{ `(${collectionInfo.taxRate}%)` }}</span>
              </template>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="合同金额(元)" prop="contractAmount">
            <div style="width: 260px">
              <el-input
                v-if="isModify"
                v-model="contractInfo.contractAmount"
                type="text"
                placeholder="合同金额"
                style="width: 250px"
                disabled
              />
              <span v-else>{{ collectionInfo.contractAmount ? toThousand(collectionInfo.contractAmount) : '' }}</span>
            </div>
          </el-form-item>
          <template v-if="isModify">
            <el-form-item label="销项税额" prop="taxRate" v-if="form.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V">
              <div style="width: 270px">
                <el-input v-model="rateMoney" type="text" placeholder="先输入税率" style="width: 180px" disabled />
                <el-input-number
                  v-model="form.taxRate"
                  :step="1"
                  :min="0"
                  :max="100"
                  :precision="DP.ACCOUNTING"
                  :controls="false"
                  controls-position="right"
                  class="input-underline"
                  style="width: 70px"
                  placeholder="0-100"
                />%
              </div>
            </el-form-item>
          </template>
          <template v-else>
            <el-form-item label="销项税额" prop="taxRate" v-if="collectionInfo.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V">
              <div style="width: 260px">
                <span>{{ rateMoney ? toThousand(rateMoney) : '' }}</span>
              </div>
            </el-form-item>
          </template>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="开票单位" prop="invoiceUnitId">
            <div style="width: 260px">
              <common-select
                v-if="isModify"
                v-model="form.invoiceUnitId"
                :options="contractInfo.companyBankAccountList"
                :type="'other'"
                :dataStructure="typeProp"
                size="small"
                clearable
                class="filter-item"
                placeholder="开票单位"
                style="width: 250px"
                @change="invoiceCompanyChange"
              />
              <span v-else>{{ collectionInfo.invoiceUnit }}</span>
            </div>
          </el-form-item>
          <el-form-item label="开票日期" prop="invoiceDate">
            <div style="width: 260px">
              <el-date-picker
                v-if="isModify"
                v-model="form.invoiceDate"
                type="date"
                value-format="x"
                placeholder="选择开票日期"
                style="width: 250px"
                :disabledDate="(date) => { return date.getTime() > new Date().getTime() }"
              />
              <template v-else>
                <span>{{ collectionInfo.invoiceDate? parseTime(collectionInfo.invoiceDate,'{y}-{m}-{d}'): '-' }}</span>
              </template>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收票单位" prop="collectionUnit">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.collectionUnit" type="text" placeholder="收票单位" style="width: 250px" />
              <span v-else>{{ collectionInfo.collectionUnit }}</span>
            </div>
          </el-form-item>
          <el-form-item label="发票号码" prop="invoiceNo">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.invoiceNo" type="text" placeholder="发票号码" style="width: 250px" />
              <span v-else>{{ collectionInfo.invoiceNo }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="发票面额(元)" prop="invoiceAmount">
            <div style="width: 260px">
              <el-input-number
                v-if="isModify"
                v-model.number="form.invoiceAmount"
                :min="-99999999999"
                :max="99999999999"
                :step="10000"
                :precision="DP.YUAN"
                placeholder="本次收款金额(元)"
                controls-position="right"
                style="width: 250px"
              />
              <span v-else>{{ collectionInfo.invoiceAmount ? toThousand(collectionInfo.invoiceAmount) : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="附件" prop="attachments">
            <div style="width: 260px">
              <upload-btn
                ref="uploadRef"
                v-if="isModify"
                v-model:files="form.attachments"
                :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
                :accept="'.pdf,.jpg,.jpeg,.png'"
                :tip="'支持扩展名:pdf .jpg .jpeg .png'"
                :limit="1"
              />
              <template v-else>
                <div v-for="item in collectionInfo.attachmentList" :key="item.id">{{ item.name }}</div>
              </template>
            </div>
          </el-form-item>
        </div>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-if="isModify"
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8 }"
            :maxlength="200"
            show-word-limit
            placeholder="可填写备注"
            style="max-width: 500px"
          />
          <span v-else>{{ collectionInfo.remark }}</span>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed, defineProps, defineEmits, nextTick } from 'vue'
import projectCascader from '@comp-base/project-cascader'
import { DP } from '@/settings/config'
import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useVisible from '@compos/use-visible'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { auditTypeEnum } from '@enum-ms/contract'
import { editStatus, edit } from '@/api/contract/collection-and-invoice/invoice'
import { ElNotification } from 'element-plus'
import UploadBtn from '@/components/file-upload/UploadBtn'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'

const formRef = ref()
const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const defaultForm = {
  id: undefined,
  collectionUnit: undefined,
  invoiceAmount: undefined,
  invoiceDate: undefined,
  invoiceNo: undefined,
  invoiceType: undefined,
  invoiceUnitId: undefined,
  invoiceUnit: undefined,
  tax: undefined,
  taxRate: undefined,
  projectId: undefined,
  remark: undefined,
  attachmentIds: [],
  attachments: undefined
}

const props = defineProps({
  collectionInfo: {
    type: Object,
    default: () => {}
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  type: {
    type: String,
    require: true
  }
})

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const contractInfo = ref({})
const isModify = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => props.collectionInfo.projectId,
  (val) => {
    if (val) {
      getContractInfoReset(val)
    }
  },
  { deep: true, immediate: true }
)

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  taxRate: [{ required: true, message: '请输入税率', trigger: 'change', type: 'number' }],
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceUnitId: [{ required: true, message: '请选择开票单位', trigger: 'change' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }],
  invoiceAmount: [{ required: true, message: '请输入发票面额', trigger: 'change', type: 'number' }]
}

function modifyInfo() {
  resetForm()
  isModify.value = true
}

function closeDrawer() {
  isModify.value = false
  handleClose()
}

function resetForm() {
  const DataValue = JSON.parse(JSON.stringify(props.collectionInfo))
  DataValue.invoiceDate = String(DataValue.invoiceDate)
  DataValue.projectId = props.collectionInfo.project.id
  DataValue.invoiceUnitId = Number(DataValue.invoiceUnitId)
  DataValue.attachments = DataValue.attachmentList ? DataValue.attachmentList.map((v) => v.id) : undefined
  form.value = DataValue
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

async function getContractInfoReset(id) {
  let data = {}
  try {
    data = await contractCollectionInfo({ projectId: id })
  } catch (e) {
    console.log('获取合同信息', e)
  } finally {
    contractInfo.value = data
  }
}

async function getContractInfo(id) {
  let data = {}
  if (isModify.value) {
    try {
      data = await contractCollectionInfo({ projectId: id })
    } catch (e) {
      console.log('获取合同信息', e)
    } finally {
      contractInfo.value = data
      form.value.collectionUnit = contractInfo.value.customerUnit
      form.value.invoiceUnitId = ''
    }
  }
}

function invoiceCompanyChange(val) {
  if (val) {
    const invoiceVal = contractInfo.value.companyBankAccountList.find((v) => v.companyId === val)
    form.value.invoiceUnit = invoiceVal.companyName
  } else {
    form.value.invoiceUnit = ''
  }
}

const rateMoney = computed(() => {
  if (isModify.value) {
    return contractInfo.value.contractAmount && form.value.taxRate
      ? ((contractInfo.value.contractAmount * form.value.taxRate) / 100).toFixed(DP.YUAN)
      : ''
  } else {
    return props.collectionInfo.tax
  }
})

function handleSuccess() {
  ElNotification({ title: '提交成功', type: 'success' })
  emit('success')
  closeDrawer()
}

async function onSubmit(val) {
  try {
    if (props.type === 'detail') {
      await formRef.value.validate()
      form.value.tax = rateMoney.value || ''
      form.value.attachmentIds = form.value.attachments ? form.value.attachments.map((v) => v.id) : undefined
      await edit(form.value)
      handleSuccess()
    } else {
      await editStatus(props.collectionInfo.id, val)
      handleSuccess()
    }
  } catch (e) {
    console.log('开票修改', e)
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
