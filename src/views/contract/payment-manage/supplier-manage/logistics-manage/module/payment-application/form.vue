<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    title="付款申请"
    :wrapper-closable="false"
    size="85%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交审核</common-button>
    </template>
    <template #content>
      <div class="detail-header">
        <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px" v-loading="crud.status.cu === 2">
          <el-row>
            <el-col :span="8">
              <el-form-item label="供应商">
              <span>{{ detailInfo.supplierName }}</span>
            </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="累计运输费">
                <span v-thousand="detailInfo.freight" />
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="累计付款">
              <span v-thousand="detailInfo.paymentAmount"/><span>（{{ (detailInfo.paymentRate).toFixed(2) }}%）</span>
            </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
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
            <el-col :span="8">
              <el-form-item label="累计收票">
                <span v-thousand="detailInfo.invoiceAmount" /><span>（{{ (detailInfo.invoiceRate).toFixed(2) }}%）</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="最后一次付款额">
                <span v-thousand="detailInfo.lastPaymentAmount" />
                <el-tag v-if="detailInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
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
            <el-col :span="8">
              <el-form-item label="本次付款"  prop="applyAmount">
                <el-input-number
                  v-model="form.applyAmount"
                  :step="10000"
                  :min="-9999999999"
                  :max="detailInfo?.sourceRow?.settlementAmount?detailInfo?.sourceRow?.settlementAmount-detailInfo?.sourceRow?.paymentAmount:9999999999"
                  :precision="DP.YUAN"
                  placeholder="本次付款"
                  controls-position="right"
                  style="width: 280px"
                  :disabled="selectionData.length===0"
                />
              </el-form-item>
            </el-col>
            <el-col :span="8">
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
          </el-row>
          <el-row>
            <el-col :span="8">
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
            <el-col :span="8">
              <el-form-item label="大写">
                <span style="color:#82848a">{{form.applyAmount?digitUppercase(form.applyAmount):''}}</span>
                <div v-if="form.applyAmount<totalFreight" style="color:red;">*让利金额<span v-thousand="totalFreight-form.applyAmount" />元</div>
              </el-form-item>
            </el-col>
            <el-col :span="8">
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
          </el-row>
          <el-row>
             <el-col :span="8">
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
            <el-col :span="8">
              <el-form-item label="付款方式" prop="paymentMethod">
                <common-select
                  v-model="form.paymentMethod"
                  :options="paymentFineModeEnum.ENUM"
                  type="enum"
                  size="small"
                  placeholder="付款方式"
                  style="width:280px;"
                  @change="paymentModeChange"
                />
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="付款行" prop="paymentBank">
                <el-select v-model="form.paymentBank" placeholder="付款行" :size="'small'" style="width: 280px" @change="bankChange" clearable :disabled="form.paymentMethod===paymentOtherModeEnum.CASH.V">
                  <el-option v-for="item in bankList" :key="item.account" :label="item.depositBank" :value="item.depositBank" />
                </el-select>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
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
            <el-col :span="8">
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
             <el-col :span="8">
              <el-form-item label="账号">
                <span>{{form.paymentBankAccount}}</span>
              </el-form-item>
            </el-col>
          </el-row>
        </el-form>
        <el-divider><span class="title">物流明细</span></el-divider>
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
            style="width: 220px"
            @change="handleDateChange"
          />
          <el-input
            v-model="query.purchaseSn"
            placeholder="采购编号"
            class="filter-item"
            style="width: 180px"
            size="small"
            clearable
          />
          <el-input
            v-model="query.branchCompanyName"
            placeholder="签订主体"
            class="filter-item"
            style="width: 180px"
            size="small"
            clearable
          />
          <el-input
            v-model="query.purchaseUserName"
            placeholder="采购员"
            class="filter-item"
            style="width: 180px"
            size="small"
            clearable
          />
          <el-input
            v-model="query.licensePlate"
            placeholder="车牌号"
            class="filter-item"
            style="width: 180px"
            size="small"
            clearable
          />
          <el-input
            v-model="query.inboundSn"
            placeholder="入库单号"
            class="filter-item"
            style="width: 180px"
            size="small"
            clearable
          />
          <common-button class="filter-item" size="small" type="success" icon="el-icon-search" @click.stop="fetchList">搜索</common-button>
          <common-button class="filter-item" size="small" type="warning" icon="el-icon-refresh" @click.stop="resetSubmit">重置</common-button>
        </div>
      </div>
      <common-table
        ref="detailRef"
        border
        :data="list"
        :max-height="420"
        style="width: 100%;margin-bottom:10px;"
        class="table-form"
        v-loading="tableLoading"
        :dataFormat="dataFormat"
        @selection-change="handleSelectionChange"
      >
        <el-table-column type="selection" align="center" width="60" class="selection" :selectable="selectable" />
        <el-table-column key="purchaseSn" prop="purchaseSn" label="采购合同编号" align="center">
          <template v-slot="scope">
            <table-cell-tag :show="scope.row.boolPayment" :name="form.auditStatus===auditTypeEnum.PASS.V?'已支付':'支付中'" :color="form.auditStatus===auditTypeEnum.PASS.V?'#67c23a':'#e6a23c'" :offset="15" />
            <span>{{scope.row.purchaseSn}}</span>
          </template>
        </el-table-column>
        <el-table-column key="branchCompanyName" prop="branchCompanyName" label="合同签订主体" align="center" min-width="150"/>
        <el-table-column key="purchaseUserName" prop="purchaseUserName" label="采购员" align="center" width="100" />
        <el-table-column prop="inboundSn" label="入库单号" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <span class="clickable" @click="openRecord(row)"> {{ row.inboundSn }}</span>
          </template>
        </el-table-column>
        <el-table-column key="freight" prop="freight" label="运输费" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="licensePlate" prop="licensePlate" label="车牌号" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="applicantName" prop="applicantName" label="入库人" align="center" :show-overflow-tooltip="true" width="100" />
      </common-table>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
      <inbound-record v-model="recordVisible" :detailInfo="currentRow" />
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, nextTick, computed, watch } from 'vue'
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
import useDecimalPrecision from '@compos/store/use-decimal-precision'
import { ElMessage } from 'element-plus'
import { auditTypeEnum } from '@enum-ms/contract'
import { paymentOtherModeEnum, paymentFineModeEnum } from '@enum-ms/finance'

import UploadBtn from '@comp/file-upload/UploadBtn'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import branchCompanySelect from '@comp-base/branch-company-select.vue'
import supplierSelect from '@comp-base/supplier-select/index.vue'
import inboundRecord from '../inbound-record'

const { decimalPrecision } = useDecimalPrecision()

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
  receiveBankAccount: undefined,
  logisticsCargoIds: [],
  paymentMethod: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const drawerRef = ref()
const pdfShow = ref(false)
const currentId = ref()
const bankList = ref([])
const recordVisible = ref(false)
const currentRow = ref({})
const query = ref({})
const detailRef = ref()
const list = ref([])
const tableLoading = ref(false)
const selectionData = ref([])

const dataFormat = ref([
  ['freight', ['to-thousand', decimalPrecision.contract]]
])

watch(
  () => crud.status.cu,
  (val) => {
    if (val > 0) {
      selectionData.value = []
      resetSubmit()
    }
  },
  { immediate: true, deep: true }
)

const totalFreight = computed(() => {
  const freightData = selectionData.value?.map((v) => v.freight)
  return freightData?.reduce((prev, curr) => {
    return prev + (curr || 0)
  }, 0) || 0
})

watch(
  () => totalFreight.value,
  (val) => {
    nextTick(() => {
      form.applyAmount = val
    })
  },
  { immediate: true, deep: true }
)

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
  paymentUnitId: [{ required: true, message: '请选择付款单位', trigger: 'change' }],
  paymentMethod: [{ required: true, message: '请选择付款方式', trigger: 'change' }]
  // paymentBank: [{ required: true, message: '请选择付款银行', trigger: 'change' }]
}

CRUD.HOOK.afterToAdd = () => {
  crud.form.actualReceivingUnitId = crud.form.actualReceivingUnitId || props.detailInfo.supplierId
}

async function fetchList() {
  const selectData = JSON.parse(JSON.stringify(selectionData.value))
  let _list = []
  tableLoading.value = true
  const params = form.id ? { ...query.value, supplierId: crud.query.supplierId, supplierPaymentId: form.id } : { ...query.value, supplierId: crud.query.supplierId }
  try {
    const { content = [] } = await logisticsIsPaymentList(params)
    _list = content
  } catch (error) {
    console.log('获取物流是否付款记录失败', error)
  } finally {
    if (selectData.length > 0) {
      selectData.forEach(v => {
        if (_list.findIndex(k => k.id === v.id) < 0) {
          _list.unshift(v)
        }
      })
    }
    list.value = _list
    if (list.value.length > 0) {
      list.value.map(v => {
        v.canSelect = !v.boolPayment
      })
      if (selectData.length > 0) {
        selectData.forEach(v => {
          const findVal = list.value.find(k => k.id === v.id)
          nextTick(() => {
            detailRef.value?.toggleRowSelection(findVal, true)
          })
        })
      }
      if (form.logisticsCargoIds.length > 0) {
        for (let i = 0; i < form.logisticsCargoIds.length; i++) {
          const findVal = list.value.find(v => v.id === form.logisticsCargoIds[i])
          if (isNotBlank(findVal)) {
            findVal.canSelect = true
            nextTick(() => {
              detailRef.value?.toggleRowSelection(findVal, true)
            })
          }
        }
      }
    }
    tableLoading.value = false
  }
}

function handleDateChange(val) {
  if (val && val.length > 1) {
    query.value.startDate = val[0]
    query.value.endDate = val[1]
  } else {
    query.value.startDate = undefined
    query.value.endDate = undefined
  }
  fetchList()
}

function resetSubmit() {
  query.value = {}
  query.value.date = []
  fetchList()
}

function selectable(row) {
  return row.canSelect
}

function handleSelectionChange(val) {
  selectionData.value = val
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

function paymentModeChange(val) {
  if (val === paymentOtherModeEnum.CASH.V) {
    crud.form.paymentBankAccount = undefined
    crud.form.paymentBank = undefined
  }
}

function bankChange(val) {
  const findVal = bankList.value.find(v => v.depositBank === val) || {}
  crud.form.paymentBankAccount = findVal.account
}

CRUD.HOOK.beforeSubmit = () => {
  if (selectionData.value.length === 0) {
    ElMessage({ message: '请勾选物流明细', type: 'error' })
    return false
  }
  crud.form.attachmentIds = crud.form.files ? crud.form.files.map((v) => v.id) : (crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined)
  crud.form.receivingUnitId = props.detailInfo.supplierId
  crud.form.logisticsCargoIds = selectionData.value.map(v => v.id)
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
.clickable {
  width: 100%;
  cursor: pointer;
  color:#409eff;
}
</style>
