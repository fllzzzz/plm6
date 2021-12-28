<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="860px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
      <div class="form-row" style="display:flex;">
        <el-form-item label="项目" prop="projectId">
          <project-cascader
            v-model="form.projectId"
            style="width:250px"
            class="filter-item"
            @change="getContractInfo(form.projectId)"
          />
        </el-form-item>
        <el-form-item label="发票类型" prop="invoiceType">
          <common-select
            v-model="form.invoiceType"
            :options="invoiceTypeEnum.ENUM"
            type="enum"
            size="small"
            clearable
            class="filter-item"
            placeholder="发票类型"
            style="width:250px"
          />
        </el-form-item>
      </div>
        <div class="form-row" style="display:flex;">
        <el-form-item label="合同金额(元)" prop="contractAmount">
          <el-input
            v-model="contractInfo.contractAmount"
            type="text"
            placeholder="合同金额"
            style="width: 250px;"
            disabled
          />
        </el-form-item>
        <el-form-item label="销项税额" prop="taxRate" v-if="form.invoiceType===invoiceTypeEnum.ENUM.SPECIAL.V">
          <el-input
            v-model="rateMoney"
            type="text"
            placeholder="先输入税率"
            style="width: 190px;"
            disabled
          />
          <el-input-number
            v-model="form.taxRate"
            :step="1"
            :min="0"
            :max="100"
            :precision="DP.ACCOUNTING"
            :controls="false"
            controls-position="right"
            class="input-underline"
            style="width:70px"
            placeholder="0-100"
          />%
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="开票单位" prop="invoiceUnitId">
          <common-select
            v-model="form.invoiceUnitId"
            :options="contractInfo.companyBankAccountList"
            :type="'other'"
            :dataStructure="typeProp"
            size="small"
            clearable
            class="filter-item"
            placeholder="开票单位"
            style="width:250px"
            @change="invoiceCompanyChange"
          />
        </el-form-item>
        <el-form-item label="开票日期" prop="invoiceDate">
          <el-date-picker
            v-model="form.invoiceDate"
            type="date"
            value-format="x"
            placeholder="选择开票日期"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="收票单位" prop="collectionUnit">
          <el-input
            v-model="form.collectionUnit"
            type="text"
            placeholder="收票单位"
            style="width: 250px;"
          />
        </el-form-item>
        <el-form-item label="发票号码" prop="invoiceNo">
          <el-input
            v-model="form.invoiceNo"
            type="text"
            placeholder="发票号码"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="发票面额(元)" prop="invoiceAmount">
          <el-input-number
            v-model.number="form.invoiceAmount"
            :min="-99999999999"
            :max="99999999999"
            :step="10000"
            :precision="DP.YUAN"
            placeholder="本次收款金额(元)"
            controls-position="right"
            style="width: 250px;"
          />
        </el-form-item>
        <el-form-item label="附件" prop="attachments">
          <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :showFileList="false"/>
        </el-form-item>
      </div>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model="form.remark"
          type="textarea"
          :autosize="{ minRows: 6, maxRows: 8}"
          :maxLength="500"
          placeholder="可填写备注"
          style="max-width: 500px;"
        />
      </el-form-item>
    </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import projectCascader from '@comp-base/project-cascader'
import { DP } from '@/settings/config'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'

const formRef = ref()
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
  attachmentIds: undefined,
  attachments: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const contractInfo = ref({})
const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  taxRate: [{ required: true, message: '请输入税率', trigger: 'change', type: 'number' }],
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceUnitId: [{ required: true, message: '请选择开票单位', trigger: 'change' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }],
  invoiceAmount: [{ required: true, message: '请输入发票面额', trigger: 'change', type: 'number' }]
}

async function getContractInfo(id) {
  let data = {}
  try {
    data = await contractCollectionInfo({ projectId: id })
  } catch (e) {
    console.log('获取合同信息', e)
  } finally {
    contractInfo.value = data
    form.collectionUnit = contractInfo.value.customerUnit
    form.invoiceUnitId = ''
  }
}

function invoiceCompanyChange(val) {
  if (val) {
    const invoiceVal = contractInfo.value.companyBankAccountList.find(v => v.companyId === val)
    form.invoiceUnit = invoiceVal.companyName
  } else {
    form.invoiceUnit = ''
  }
}

const rateMoney = computed(() => {
  return contractInfo.value.contractAmount && form.taxRate ? ((contractInfo.value.contractAmount * form.taxRate) / 100).toFixed(DP.YUAN) : ''
})

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.tax = rateMoney.value || ''
  crud.form.attachmentIds = crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
