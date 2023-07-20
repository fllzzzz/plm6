<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    title="付款申请"
    :wrapper-closable="false"
    size="80%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交审核</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px">
        <el-row>
          <el-col :span="12">
            <el-form-item label="供应商">
            <span>{{ detailInfo.supplierName }}</span>
          </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="累计运输费">
              <span v-thousand="detailInfo.freight" />
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="实际收款单位">
              <supplier-select
                v-model="form.actualReceivingUnitId"
                clearable
                placeholder="可搜索"
                show-hide
                :basicClass="supplierClassEnum.LOGISTICS.V"
                style="width: 280px"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="累计付款">
            <span v-thousand="detailInfo.paymentAmount"/><span>（{{ (detailInfo.paymentRate).toFixed(2) }}%）</span>
          </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="累计收票">
              <span v-thousand="detailInfo.invoiceAmount"/><span>（{{ (detailInfo.invoiceRate).toFixed(2) }}%）</span>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="最后一次付款额">
              <span v-thousand="detailInfo.lastPaymentAmount" />
              <el-tag v-if="detailInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="开户行" prop="receiveBank">
              <el-input
                v-model.trim="form.receiveBank"
                type="text"
                style="width: 280px"
                maxlength="50"
                placeholder="请输入开户行"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="账号" prop="receiveBankAccount">
              <el-input
                v-model.trim="form.receiveBankAccount"
                type="text"
                style="width: 280px"
                maxlength="50"
                placeholder="请输入账号"
              />
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="付款日期" prop="paymentDate">
              <el-date-picker
                v-model="form.paymentDate"
                type="date"
                value-format="x"
                placeholder="选择付款日期"
                style="width: 280px"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="本次付款"  prop="applyAmount">
              <el-input-number
                v-model="form.applyAmount"
                :step="10000"
                :min="-9999999999"
                :max="detailInfo?.sourceRow?.settlementAmount?detailInfo?.sourceRow?.settlementAmount-detailInfo?.sourceRow?.paymentAmount:999999999999"
                :precision="DP.YUAN"
                placeholder="本次付款"
                controls-position="right"
                style="width: 280px"
              />
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="付款事由" prop="paymentReasonId">
            <common-select
              v-model="form.paymentReasonId"
              :options="dict.payment_reason"
              type="dict"
              size="small"
              clearable
              placeholder="付款事由"
              style="width: 280px"
            />
          </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="大写">
              <span style="color:#82848a">{{form.applyAmount?digitUppercase(form.applyAmount):''}}</span>
              <span v-if="form.applyAmount<detailInfo.freight" style="color:red;">*让利金额{{detailInfo.freight-form.applyAmount}}元</span>
            </el-form-item>
        </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="付款单位" prop="paymentUnitId">
              <branch-company-select
                v-model="form.paymentUnitId"
                default
                placeholder="付款单位"
                style="width: 280px"
                @companyChange="companyChange"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="付款行" prop="paymentBank">
              <el-select v-model="form.paymentBank" placeholder="付款行" :size="'small'" style="width: 280px" @change="bankChange">
                <el-option v-for="item in bankList" :key="item.account" :label="item.depositBank" :value="item.depositBank" />
              </el-select>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
             <el-form-item label="备注" prop="remark">
              <el-input
                v-model="form.remark"
                type="textarea"
                style="width: 100%"
                maxlength="200"
                show-word-limit
                :autosize="{ minRows: 2, maxRows: 4 }"
                placeholder="请输入备注"
              />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="账号">
              <span>{{form.paymentBankAccount}}</span>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="12">
            <el-form-item label="付款凭证">
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
      <el-divider><span class="title">物流信息</span></el-divider>
      <div class="head-container">
        <el-date-picker
          v-model="query.date"
          type="daterange"
          range-separator=":"
          size="small"
          value-format="x"
          class="filter-item date-item"
          start-placeholder="开始时间"
          end-placeholder="结束时间"
          style="width: 240px"
          @change="handleDateChange"
        />
        <el-input
          v-model="query.purchaseSn"
          placeholder="采购编号"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.branchCompanyName"
          placeholder="签订主体"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.purchaseUserName"
          placeholder="采购员"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.licensePlate"
          placeholder="车牌号"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <el-input
          v-model="query.inboundSn"
          placeholder="入库单号"
          class="filter-item"
          style="width: 200px"
          size="small"
          clearable
        />
        <common-button class="filter-item" size="small" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
        <common-button class="filter-item" size="small" type="warning" icon="el-icon-refresh" @click.stop="resetSubmit">重置</common-button>
      </div>
      <common-table
        ref="detailRef"
        border
        :data="list"
        :max-height="maxHeight"
        style="width: 100%;margin-bottom:10px;"
        class="table-form"
        v-loading="tableLoading"
      >
        <el-table-column key="projectName" prop="projectName" label="采购合同编号" align="center" />
        <el-table-column key="projectName" prop="projectName" label="合同签订主体" align="center" />
        <el-table-column key="projectName" prop="projectName" label="采购员" align="center" />
        <el-table-column prop="inboundSn" label="入库单号" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <span class="clickable" @click="openRecord(row)"> {{ row.inboundSn }}</span>
          </template>
        </el-table-column>
        <el-table-column key="freight" prop="freight" label="运输费" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="paymentAmount" prop="paymentAmount" label="车牌号" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="paymentAmount" prop="paymentAmount" label="入库人" align="center" :show-overflow-tooltip="true" />
      </common-table>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
      <inbound-record v-model="recordVisible" :detailInfo="currentRow" />
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, nextTick } from 'vue'
import { logisticsIsPaymentList } from '@/api/contract/supplier-manage/jd-logistics-payment'

import moment from 'moment'
import { fileClassifyEnum } from '@enum-ms/file'
import { digitUppercase } from '@data-type/number'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { supplierClassEnum } from '@enum-ms/supplier'
import { regForm } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import { isNotBlank } from '@/utils/data-type'

import UploadBtn from '@comp/file-upload/UploadBtn'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import inboundRecord from '../inbound-record'

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
  actualReceivingUnitId: undefined,
  applyAmount: undefined,
  attachmentIds: [
  ],
  files: [],
  paymentBank: undefined,
  paymentBankAccount: undefined,
  paymentReasonId: undefined,
  paymentUnitId: undefined,
  projectId: undefined,
  receivingUnitId: undefined,
  remark: undefined,
  receiveBank: undefined,
  receiveBankAccount: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const pdfShow = ref(false)
const currentId = ref()
const bankList = ref([])
const recordVisible = ref(false)
const currentRow = ref({})
const query = ref({})
const list = ref([])
const tableLoading = ref(false)

const validateMoney = (rule, value, callback) => {
  if (!isNotBlank(value)) {
    callback(new Error('请填写申请金额'))
  }
  callback()
}

const rules = {
  paymentDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }],
  paymentReasonId: [{ required: true, message: '请选择付款事由', trigger: 'change' }],
  applyAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }],
  paymentUnitId: [{ required: true, message: '请选择付款单位', trigger: 'change' }]
  // paymentBank: [{ required: true, message: '请选择付款银行', trigger: 'change' }]
}

CRUD.HOOK.beforeToAdd = () => {
  fetchList()
}

CRUD.HOOK.afterToAdd = () => {
  crud.form.actualReceivingUnitId = crud.form.actualReceivingUnitId || props.detailInfo.supplierId
}

async function fetchList() {
  let _list = []
  tableLoading.value = true
  const params = form.id ? { ...query.value, supplierId: crud.query.supplierId, supplierPaymentId: form.id } : { ...query.value, supplierId: crud.query.supplierId }
  try {
    const { content = [] } = await logisticsIsPaymentList(params)
    _list = content
  } catch (error) {
    console.log('获取物流是否付款记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

function resetSubmit() {
  query.value = {}
  query.value.date = []
  fetchList()
}

// 打开入库记录
function openRecord(row) {
  currentRow.value = row.sourceRow
  nextTick(() => {
    recordVisible.value = true
  })
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

function companyChange(val) {
  bankList.value = val.bankAccountList || []
}

function bankChange(val) {
  const findVal = bankList.value.find(v => v.depositBank === val) || {}
  crud.form.paymentBankAccount = findVal.account
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.attachmentIds = crud.form.files ? crud.form.files.map((v) => v.id) : (crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined)
  crud.form.receivingUnitId = props.detailInfo.supplierId
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
