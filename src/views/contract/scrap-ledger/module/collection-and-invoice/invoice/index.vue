<template>
  <div class="app-container">
    <!--表格渲染-->
    <div>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right:10px;" v-permission="permission.add">添加</common-button>
      <!-- <el-tag type="success" size="medium" v-if="contractInfo.contractAmount">{{'合同金额:'+toThousand(contractInfo.contractAmount,decimalPrecision.contract)}}</el-tag>
      <el-tag type="success" size="medium" v-if="currentRow.settlementAmount" style="margin-left:5px;">{{'结算金额:'+toThousand(currentRow.settlementAmount,decimalPrecision.contract)}}</el-tag> -->
      <print-table
        v-permission="crud.permission.print"
        api-key="scrapInvoice"
        :params="crud.query"
        style="float: right"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      :cell-class-name="wrongCellMask"
      :stripe="false"
      return-source-data
      show-summary
      :summary-method="getSummaries"
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="invoiceDate" prop="invoiceDate" label="开票日期" align="center" width="125">
        <template v-slot="scope">
          <el-date-picker
            v-if="scope.row.isModify"
            v-model="scope.row.invoiceDate"
            type="date"
            size="small"
            value-format="x"
            placeholder="选择日期"
            style="width:100%"
          />
          <template v-else>
            <div>{{ scope.row.invoiceDate? parseTime(scope.row.invoiceDate,'{y}-{m}-{d}'): '-' }}</div>
          </template>
        </template>
      </el-table-column>
      <el-table-column key="invoiceAmount2" prop="invoiceAmount2" label="开票额" align="center" min-width="220" class="money-column">
        <el-table-column key="invoiceAmount" prop="invoiceAmount" label="金额" align="center" min-width="110">
          <template v-slot="scope">
            <el-input-number
                v-if="scope.row.isModify"
                v-show-thousand
                v-model.number="scope.row.invoiceAmount"
                :min="-9999999999"
                :max="9999999999"
                :step="100"
                :precision="decimalPrecision.contract"
                placeholder="开票额(元)"
                controls-position="right"
                @change="moneyChange(scope.row)"
              />
              <div v-else>{{ isNotBlank(scope.row.invoiceAmount) ? toThousand(scope.row.invoiceAmount,decimalPrecision.contract): '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column key="invoiceAmount1" prop="invoiceAmount1" label="大写" align="center" min-width="110" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <div>{{isNotBlank(scope.row.invoiceAmount)?digitUppercase(scope.row.invoiceAmount):'-'}}</div>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="invoiceType" prop="invoiceType" label="发票类型" align="center" width="110">
        <template v-slot="scope">
          <div>{{ scope.row.invoiceType? invoiceTypeEnum.VL[scope.row.invoiceType]: '-' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="70">
        <template v-slot="scope">
          <div v-if="scope.row.invoiceType !== invoiceTypeEnum.RECEIPT.V && currentRow.isTax !== isTaxContractEnum.NO.V && scope.row.isModify">
            <el-input-number
              v-model="scope.row.taxRate"
              :step="1"
              :min="0"
              :max="100"
              :precision="0"
              :controls="false"
              controls-position="right"
              class="input-underline"
              style="width: 50px; text-align: center"
              placeholder="0-100"
              @change="taxMoney(scope.row)"
            />%
          </div>
          <div v-else>{{ scope.row.taxRate? scope.row.taxRate+'%': '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="noTaxAmount" prop="noTaxAmount" label="不含税金额" align="center" width="70">
        <template v-slot="scope">
          <span>{{isNotBlank(scope.row.noTaxAmount) ? toThousand(scope.row.noTaxAmount,decimalPrecision.contract): '-'}}</span>
        </template>
      </el-table-column>
      <el-table-column key="invoiceUnit" prop="invoiceUnit" label="开票单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.invoiceUnit }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionUnit" prop="collectionUnit" label="收票单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <el-input
            v-if="scope.row.isModify"
            v-model.trim="scope.row.collectionUnit"
            placeholder="收票单位"
            style="width:100%;"
            maxlength="50"
          />
          <div v-else>{{ scope.row.collectionUnit  }}</div>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceNo" label="发票号码" align="center" min-width="130">
        <template v-slot="scope">
          <el-input v-if="scope.row.isModify" v-model.trim="scope.row.invoiceNo" type="text" placeholder="发票号码" style="width: 100%;" />
          <span v-else>{{ scope.row.invoiceNo  }}</span>
        </template>
      </el-table-column>
       <el-table-column prop="invoiceContent" label="发票内容" align="center" min-width="130">
        <template v-slot="scope">
          <el-input v-if="scope.row.isModify" v-model.trim="scope.row.invoiceContent" type="text" placeholder="发票内容" style="width: 100%;"/>
          <span v-else>{{ scope.row.invoiceContent  }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="remark" label="备注" align="center" min-width="100">
        <template v-slot="scope">
          <el-input v-if="scope.row.isModify" v-model.trim="scope.row.remark" type="textarea" placeholder="备注" style="width: 100%;" maxlength="200" />
          <span v-else>{{ scope.row.remark  }}</span>
        </template>
      </el-table-column>
      <el-table-column key="writtenByName" prop="writtenByName" label="办理人" align="center" width="80px">
        <template v-slot="scope">
          <div>{{ scope.row.writtenByName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditorName" prop="auditorName" label="审核人" align="center" width="80px">
        <template v-slot="scope">
          <div>{{ scope.row.auditorName }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([ ...permission.edit,...permission.audit])"
        label="操作"
        width="190px"
        align="center"
      >
        <template v-slot="scope">
          <template v-if="!scope.row.isModify">
            <common-button icon="el-icon-edit" type="primary" size="mini" @click="modifyRow(scope.row)" v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.edit)"/>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定删除吗?"
              @confirm="rowDelete(scope.row)"
              v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.del)"
            >
              <template #reference>
                <common-button icon="el-icon-delete" type="danger" size="mini" />
              </template>
            </el-popconfirm>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定通过吗?"
              @confirm="passConfirm(scope.row)"
              v-if="scope.row.auditStatus===auditTypeEnum.AUDITING.V && checkPermission(permission.audit)"
            >
              <template #reference>
                <common-button type="success" size="mini" >通过</common-button>
              </template>
            </el-popconfirm>
            <el-tag type="success" v-if="scope.row.auditStatus===auditTypeEnum.PASS.V" class="pass-tag">已复核</el-tag>
          </template>
          <template v-else>
            <el-popconfirm
              confirm-button-text="确定"
              cancel-button-text="取消"
              title="确定取消吗?"
              @confirm="rowCancel(scope.row,scope.$index)"
            >
              <template #reference>
                <common-button type="primary" size="mini">取消</common-button>
              </template>
            </el-popconfirm>
            <common-button type="info" plain size="mini" @click="rowSubmit(scope.row)">保存</common-button>
          </template>
        </template>
      </el-table-column>
    </common-table>
  <!--分页组件-->
  <pagination />
  <mForm :existInvoiceNo="invoiceNoArr" :projectId="projectId" :currentRow="currentRow"/>
  </div>
</template>

<script setup>
// import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'
import crudApi, { editStatus } from '@/api/contract/scrap-collection-invoice/invoice'
import { ref, defineEmits, defineProps, watch, provide, nextTick } from 'vue'
import { ElMessage } from 'element-plus'

import checkPermission from '@/utils/system/check-permission'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { isTaxContractEnum, auditTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { toThousand, digitUppercase } from '@data-type/number'
import { validate } from '@compos/form/use-table-validate'
import { isNotBlank } from '@data-type/index'
import { scrapLedgerPM } from '@/page-permission/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import pagination from '@crud/Pagination'
import mForm from './form'

const { decimalPrecision } = useDecimalPrecision()

const permission = scrapLedgerPM.invoice

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  projectId: {
    type: [String, Number],
    default: undefined
  },
  visibleValue: {
    type: Boolean,
    default: false
  }
})
const tableRef = ref()
const contractInfo = ref({})
const originRow = ref({})
const bankList = ref([])
const totalAmount = ref(0)
const invoiceNoArr = ref([])
const emit = defineEmits(['success'])
provide('contractInfo', contractInfo)
provide('totalAmount', totalAmount)
const { crud, CRUD } = useCRUD(
  {
    title: '开票填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['haveInvoiceAmount', 'collectionMode', 'invoiceType', 'collectionDepositBank', 'collectionBankAccount', 'paymentBankAccount', 'paymentDepositBank', 'auditorName', 'auditTime'],
    hasPagination: true,
    params: {}
  },
  tableRef
)

const tableRules = {
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceAmount: [{ required: true, message: '请选择开票额', trigger: 'change', type: 'number' }],
  taxRate: [{ required: true, message: '请输入税率', trigger: 'blur' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceNo: [{ required: true, message: '请输入发票号', trigger: 'blur' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }]
}

const otherRules = {
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceAmount: [{ required: true, message: '请选择开票额', trigger: 'change', type: 'number' }],
  // invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceNo: [{ required: true, message: '请输入发票号', trigger: 'blur' }],
  collectionUnit: [{ required: true, message: '请输入收票单位', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  let rules = {}
  if (row.invoiceType !== invoiceTypeEnum.RECEIPT.V && props.currentRow.isTax !== isTaxContractEnum.NO.V) {
    rules = tableRules
  } else {
    rules = otherRules
  }
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.currentRow,
  (val) => {
    console.log(val)
    bankList.value = []
    contractInfo.value = {}
    if (val) {
      getContractInfo(val.branchCompanyId)
    }
    crud.toQuery()
  },
  { deep: true, immediate: true }
)

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

async function getContractInfo(id) {
  // let data = {}
  try {
    // data = await contractCollectionInfo({ projectId: id })
  } catch (e) {
    console.log('获取合同信息', e)
  } finally {
    contractInfo.value = props.currentRow
  }
}

// function invoiceTypeChange(row) {
//   row.taxRate = undefined
// }
function moneyChange(row) {
  if (props.currentRow.settlementAmount) {
    totalAmount.value = 0
    crud.data.map(v => {
      if (v.invoiceAmount) {
        totalAmount.value += v.invoiceAmount
      }
    })
    if (totalAmount.value > props.currentRow.settlementAmount) {
      const num = row.invoiceAmount - (totalAmount.value - props.currentRow.settlementAmount)
      // 解决修改失效
      nextTick(() => {
        row.invoiceAmount = num || 0
        taxMoney(row)
        totalAmount.value = 0
        crud.data.map(v => {
          if (v.invoiceAmount) {
            totalAmount.value += v.invoiceAmount
          }
        })
      })
    }
  }
  taxMoney(row)
}

function taxMoney(row) {
  if (isNotBlank(row.invoiceAmount) && row.taxRate) {
    row.tax = row.invoiceAmount * row.taxRate / 100
    row.noTaxAmount = (row.invoiceAmount / (1 + row.taxRate / 100)).toFixed(decimalPrecision.value.contract)
  } else {
    if (row.invoiceType === invoiceTypeEnum.RECEIPT.V) {
      row.noTaxAmount = row.invoiceAmount
    }
  }
}
// function checkInvoiceNo(row) {
//   if (row.invoiceNo) {
//     const val = invoiceNoArr.value.find(v => v.dataIndex === row.dataIndex)
//     if (invoiceNoArr.value.findIndex(v => v.invoiceNo === row.invoiceNo) > -1) {
//       ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
//       row.invoiceNo = undefined
//       if (val) {
//         val.invoiceNo = undefined
//       }
//     } else {
//       if (val) {
//         val.invoiceNo = row.invoiceNo
//       } else {
//         invoiceNoArr.value.push({
//           invoiceNo: row.invoiceNo,
//           dataIndex: row.dataIndex
//         })
//       }
//     }
//   }
// }

async function passConfirm(row) {
  try {
    await editStatus({ id: row.id, auditStatus: auditTypeEnum.PASS.V })
    crud.notify(`审核成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
    emit('success')
  } catch (e) {
    console.log('审核失败', e)
  }
}

function modifyRow(row) {
  originRow.value = JSON.parse(JSON.stringify(row))
  row.isModify = true
  row.invoiceDate = String(row.invoiceDate)
}

async function rowDelete(row) {
  try {
    await crudApi.del(row.id)
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log(`删除失败`, e)
  }
}
function rowCancel(row) {
  row.isModify = false
  if (row.id) {
    row = Object.assign(row, JSON.parse(JSON.stringify(originRow.value)))
  } else {
    const index = crud.data.findIndex(v => v.dataIndex === row.dataIndex)
    crud.data.splice(index, 1)
  }
}

async function rowSubmit(row) {
  if (row.invoiceAmount === 0) {
    ElMessage.error('开票额必须大于0')
    return
  }
  let rules = {}
  if (row.invoiceType !== invoiceTypeEnum.RECEIPT.V && props.currentRow.isTax !== isTaxContractEnum.NO.V) {
    rules = tableRules
  } else {
    rules = otherRules
  }
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row)
    if (!row.verify[rule]) {
      flag = false
    }
  }
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return
  }
  const messageName = row.id ? '修改' : '新增'
  try {
    if (row.id) {
      await crudApi.edit(row)
    } else {
      await crudApi.add(row)
    }
    crud.notify(`${messageName}成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (e) {
    console.log(messageName, e)
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['invoiceAmount', decimalPrecision.value.contract]],
    toThousandFields: ['invoiceAmount']
  })
}

CRUD.HOOK.beforeRefresh = () => {
  // crud.query.projectId = props.projectId
  crud.query.branchCompanyId = props.currentRow.branchCompanyId
  crud.query.contractWasteId = props.currentRow.contractWasteId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  totalAmount.value = 0
  invoiceNoArr.value = []
  data.data.content.map(v => {
    // v.projectId = v.project.id
    v.dataIndex = v.id + 'id'
    if (v.invoiceAmount) {
      totalAmount.value += v.invoiceAmount
    }
    if (v.invoiceNo) {
      invoiceNoArr.value.push({
        invoiceNo: v.invoiceNo,
        dataIndex: v.dataIndex
      })
    }
    v.noTaxAmount = v.invoiceType !== invoiceTypeEnum.RECEIPT.V ? (v.invoiceAmount / (1 + v.taxRate / 100)).toFixed(decimalPrecision.value.contract) : v.invoiceAmount
  })
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
.collection-table{
  ::v-deep(.el-select .el-input__inner){
    padding-left:2px !important;
    padding-right:5px !important;
  }
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
    padding:0 5px !important;
  }
  ::v-deep(.el-input__inner) {
    text-align: left;
    padding:0 5px !important;
  }
  ::v-deep(.el-date-editor .el-input__inner){
    padding-left:25px !important;
  }
  ::v-deep(.el-table__cell .cell){
    padding-left:2px;
    padding-right:2px;
  }
}

::v-deep(.pass-tag){
  padding:0 50px;
}
</style>
