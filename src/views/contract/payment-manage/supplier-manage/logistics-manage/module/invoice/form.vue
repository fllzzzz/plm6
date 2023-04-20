<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="60%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <el-row>
          <el-col :span="12">
            <el-form-item label="购买方" prop="branchCompanyId">
              <branch-company-select
                v-model="form.branchCompanyId"
                default
                placeholder="购买方"
                style="width: 280px"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="销售方" prop="supplierName">
              <span>{{ detailInfo.supplierName }}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="发票及税率" prop="invoiceType" class="form-label-require">
              <invoice-type-select
                v-model:invoiceType="form.invoiceType"
                v-model:taxRate="form.taxRate"
                style="width: 360px"
                :classification="classification"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="实际开票单位" prop="actualInvoiceUnitId">
              <supplier-select
                v-model="form.actualInvoiceUnitId"
                clearable
                placeholder="可搜索"
                :basicClass="supplierClassEnum.LOGISTICS.V"
                show-hide
                style="width: 280px"
              />
            </el-form-item>
          </el-col>

        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="发票面额" prop="invoiceAmount">
              <el-input-number
                v-show-thousand
                v-model.number="form.invoiceAmount"
                :min="0"
                :max="999999999999"
                :step="100"
                :precision="DP.YUAN"
                placeholder="收票额(元)"
                controls-position="right"
                style="width: 280px"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="发票号" prop="invoiceSerialNumber">
              <el-input v-model.trim="form.invoiceSerialNumber" type="text" placeholder="发票号码" style="width: 280px;" maxlength="20"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="收票日期" prop="receiveInvoiceDate">
              <el-date-picker
                v-model="form.receiveInvoiceDate"
                type="date"
                size="small"
                value-format="x"
                placeholder="选择日期"
                :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
                style="width:280px;"
              />
            </el-form-item>
             <el-form-item label="备注" prop="remark">
              <el-input
                v-model="form.remark"
                type="textarea"
                style="width: 280px"
                maxlength="200"
                show-word-limit
                :autosize="{ minRows: 2, maxRows: 4 }"
                placeholder="请输入备注"
              />
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="发票凭证">
              <template #label>
                付款凭证
                <el-tooltip
                  effect="light"
                  :content="`双击可预览附件`"
                  placement="top"
                  v-if="form.id && form.attachments?.length && !form.files?.length"
                >
                  <i class="el-icon-info" />
                </el-tooltip>
              </template>
              <template v-if="form.id && form.attachments?.length && !form.files?.length">
                <div v-for="item in form.attachments" :key="item.id">
                  <div style="cursor:pointer;color:#409eff;" @dblclick="attachmentView(item)">{{item.name}}</div>
                </div>
              </template>
              <upload-btn ref="uploadRef" v-model:files="form.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.jpg,.png,.pdf,.jpeg'"/>
            </el-form-item>
          </el-col>
        </el-row>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'

import { DP } from '@/settings/config'
import { fileClassifyEnum } from '@enum-ms/file'
import { supplierClassEnum } from '@enum-ms/supplier'

import UploadBtn from '@comp/file-upload/UploadBtn'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import invoiceTypeSelect from '@comp-base/invoice-type-select.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const formRef = ref()
const defaultForm = {
  actualInvoiceUnitId: undefined,
  attachmentIds: [],
  files: [],
  branchCompanyId: undefined,
  invoiceAmount: undefined,
  invoiceSerialNumber: undefined,
  invoiceType: undefined,
  projectId: undefined,
  receiveInvoiceDate: undefined,
  remark: undefined,
  supplierId: undefined,
  taxRate: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const pdfShow = ref(false)
const currentId = ref()

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

CRUD.HOOK.afterToAdd = () => {
  crud.form.actualInvoiceUnitId = crud.form.actualInvoiceUnitId || props.detailInfo.supplierId
}

// 金额校验
const validateMoney = (value, row) => {
  if (!value) return false
  return true
}

const rules = {
  branchCompanyId: [{ required: true, message: '请选择购买方', trigger: 'change' }],
  invoiceType: [{ required: true, message: '请选择发票及税率', trigger: 'change' }],
  actualInvoiceUnitId: [{ required: true, message: '请选择实际开票单位', trigger: 'change' }],
  paymentDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }],
  invoiceAmount: [{ required: true, validator: validateMoney, trigger: 'change' }],
  invoiceSerialNumber: [{ required: true, message: '请输入发票号码', trigger: 'blur' }],
  receiveInvoiceDate: [{ required: true, message: '请选择收票日期', trigger: 'change' }]
}

const classification = supplierClassEnum.LOGISTICS.V

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.attachmentIds = crud.form.files ? crud.form.files.map((v) => v.id) : (crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined)
  crud.form.supplierId = props.detailInfo.supplierId
}

</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
