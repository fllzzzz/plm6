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
        <el-form-item :label="form.id?'本次项目违法处罚':'项目违法处罚'" prop="notBoolDeduct">
          <span v-thousand="form.breachAmount"  v-empty-text v-if="form.id"/>
          <span v-thousand="detailInfo?.penalty"  v-empty-text v-else/>
          <el-checkbox v-model="form.notBoolDeduct" label="本次不扣除" size="small" style="margin-left:20px;vertical-align:middle;" :disabled="!detailInfo.penalty"/>
          <span v-if="form.id && (detailInfo.penalty !== form.breachAmount) && !form.notBoolDeduct" style="color:red;margin-left:10px;">(*最新罚款:<span v-thousand="detailInfo?.penalty" v-empty-text/> <el-checkbox v-model="form.boolUpdateBreachAmount" label="是否扣除最新" size="small" style="margin-left:10px;vertical-align:middle;" />)</span>
        </el-form-item>
        <el-form-item label="本次付款"  prop="inputAmount">
          <el-input-number
              v-model="form.inputAmount"
              :step="10000"
              :min="-9999999999"
              :max="detailInfo?.sourceRow?.settlementAmount?detailInfo?.sourceRow?.settlementAmount-detailInfo?.sourceRow?.paymentAmount:9999999999"
              :precision="DP.YUAN"
              placeholder="本次付款"
              controls-position="right"
              style="width: 220px"
            />
        </el-form-item>
        <el-form-item label="大写">
            <span style="color:#82848a">{{form.inputAmount?digitUppercase(form.inputAmount):''}}</span>
        </el-form-item>
         <el-form-item label="收款银行">
          <span>{{detailInfo?.supplier.bankName}}</span>
        </el-form-item>
        <el-form-item label="账号">
          <span>{{detailInfo?.supplier.bankAccount}}</span>
        </el-form-item>
        <el-form-item label="申请金额">
          <span v-thousand="payableMoney" />
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
import { ref, defineProps, computed } from 'vue'

import moment from 'moment'
import { fileClassifyEnum } from '@enum-ms/file'
import { digitUppercase } from '@data-type/number'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { isNotBlank } from '@data-type/index'

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
  breachAmount: undefined, // 本次扣除金额
  notBoolDeduct: false, // 本次不扣除
  boolUpdateBreachAmount: undefined, // 编辑时是否修改违约金
  boolDeduct: undefined,
  inputAmount: undefined,
  attachmentIds: undefined,
  files: [],
  orderId: undefined,
  paymentReasonId: undefined

}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const pdfShow = ref(false)
const currentId = ref()

const payableMoney = computed(() => {
  if (form.id) {
    if (form.notBoolDeduct) {
      return form.inputAmount
    } else {
      return isNotBlank(form.inputAmount) ? form.inputAmount - (form.boolUpdateBreachAmount ? (props.detailInfo.penalty ? props.detailInfo.penalty : 0) : (form.breachAmount ? form.breachAmount : 0)) : 0
    }
  } else {
    if (form.notBoolDeduct) {
      return form.inputAmount
    } else {
      return isNotBlank(form.inputAmount) ? form.inputAmount - (props.detailInfo?.penalty ? props.detailInfo.penalty : 0) : 0
    }
  }
})
const validateMoney = (rule, value, callback) => {
  if (!isNotBlank(value)) {
    callback(new Error('请填写本次金额'))
  }
  callback()
}

const rules = {
  paymentDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }],
  paymentReasonId: [{ required: true, message: '请选择付款事由', trigger: 'change' }],
  inputAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }]
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

// 新增
CRUD.HOOK.afterToEdit = () => {
  setTimeout(() => {
    form.notBoolDeduct = !form.boolDeduct
    form.inputAmount = form.applyAmount + form.breachAmount
    form.boolUpdateBreachAmount = false
  }, 300)
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.attachmentIds = crud.form.files ? crud.form.files.map((v) => v.id) : (crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined)
  crud.form.boolDeduct = isNotBlank(crud.form.notBoolDeduct) ? !crud.form.notBoolDeduct : (props.detailInfo?.penalty ? undefined : false)
  crud.form.orderId = props.detailInfo.id
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
