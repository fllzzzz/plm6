<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    title="付款申请"
    :wrapper-closable="false"
    size="40%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交审核</common-button>
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
          <span  v-thousand="detailInfo.amount" />
        </el-form-item>
        <el-form-item label="入库额">
          <span  v-thousand="detailInfo.inboundAmount" />
        </el-form-item>
        <el-form-item label="结算额" v-if="detailInfo?.sourceRow?.settlementAmount">
          <span v-thousand="detailInfo.settlementAmount" />
        </el-form-item>
        <el-form-item label="已付款">
          <span v-thousand="detailInfo.paymentAmount"/><span>（{{ detailInfo.paymentRate }}%）</span>
        </el-form-item>
        <el-form-item label="已收票">
          <span v-thousand="detailInfo.invoiceAmount"/><span>（{{ detailInfo.invoiceRate }}%）</span>
        </el-form-item>
        <el-form-item label="最后一次付款">
          <span  v-thousand="detailInfo.lastPaymentAmount" />
          <el-tag v-if="detailInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
        </el-form-item>
        <el-form-item label="本次付款"  prop="applyAmount">
          <el-input-number
              v-model="form.applyAmount"
              :step="10000"
              :min="0"
              :max="detailInfo?.sourceRow?.settlementAmount?detailInfo?.sourceRow?.settlementAmount-detailInfo?.sourceRow?.paymentAmount:999999999999"
              :precision="DP.YUAN"
              placeholder="本次付款"
              controls-position="right"
              style="width: 220px"
            />
        </el-form-item>
        <el-form-item label="大写">
            <span style="color:#82848a">{{form.applyAmount?digitUppercase(form.applyAmount):''}}</span>
        </el-form-item>
        <el-form-item label="申请日期" prop="paymentDate">
          <el-date-picker
            v-model="form.paymentDate"
            type="date"
            value-format="x"
            placeholder="选择申请日期"
            :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
            style="width: 220px"
          />
        </el-form-item>
        <el-form-item label="付款事由" prop="paymentReasonId">
          <common-select
            v-model="form.paymentReasonId"
            :options="dict.payment_reason"
            type="dict"
            size="small"
            clearable
            placeholder="付款事由"
            style="width: 220px"
          />
        </el-form-item>
        <el-form-item label="收款单位">
          <span>{{detailInfo.supplierBankName}}</span>
        </el-form-item>
        <el-form-item label="收款银行">
          <span>{{detailInfo.supplierBankAccount}}</span>
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            style="width: 100%"
            maxlength="200"
            :autosize="{ minRows: 2, maxRows: 4 }"
            placeholder="请输入备注"
          />
        </el-form-item>
        <el-form-item label="附件">
          <template #label>
            附件
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
              <div style="cursor:pointer;" @dblclick="attachmentView(item)">{{item.name}}</div>
            </div>
          </template>
          <upload-btn ref="uploadRef" v-model:files="form.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.jpg,.png,.pdf,.jpeg'"/>
        </el-form-item>
      </el-form>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'

import moment from 'moment'
import { fileClassifyEnum } from '@enum-ms/file'
import { digitUppercase } from '@data-type/number'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'

import { regForm } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import UploadBtn from '@comp/file-upload/UploadBtn'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'

const formRef = ref()
const dict = useDict(['payment_reason'])
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const defaultForm = {
  paymentDate: moment().startOf('day').format('x'), // 默认当天0点的时间戳
  applyAmount: undefined,
  attachmentIds: undefined,
  files: [],
  orderId: undefined,
  paymentReasonId: undefined,
  propertyType: undefined,
  receiveBank: undefined,
  receiveBankAccount: undefined,
  remark: ''
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const pdfShow = ref(false)
const currentId = ref()

const validateMoney = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请填写申请金额并大于0'))
  }
  callback()
}

const rules = {
  paymentDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }],
  paymentReasonId: [{ required: true, message: '请选择付款事由', trigger: 'change' }],
  applyAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }]
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.attachmentIds = crud.form.files ? crud.form.files.map((v) => v.id) : (crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined)
  crud.form.orderId = props.detailInfo.id
  crud.form.propertyType = props.detailInfo.propertyType
  crud.form.receiveBank = props.detailInfo.supplierBankName || undefined
  crud.form.receiveBankAccount = props.detailInfo.supplierBankAccount || undefined
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
