<template>
  <div class="app-container">
    <!--表格渲染-->
    <div>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right:10px;">添加</common-button>
      <el-tag type="success" size="medium" v-if="currentRow.amount">{{'合同额:'+toThousand(currentRow.amount)}}</el-tag>
    </div>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%;margin-top:10px;"
      class="collection-table"
      return-source-data
      :showEmptySymbol="false"
      :cell-class-name="wrongCellMask"
      :stripe="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="receiveInvoiceDate" prop="receiveInvoiceDate" label="*开票日期" align="center" width="160">
        <template v-slot="scope">
          <template v-if="scope.row.type===2">
            <span>合计</span>
          </template>
          <template v-else>
            <el-date-picker
              v-if="scope.row.isModify"
              v-model="scope.row.receiveInvoiceDate"
              type="date"
              size="small"
              value-format="x"
              placeholder="选择日期"
              style="width:100%"
              :disabledDate="(date) => {if (scope.row.receiveInvoiceDate) { return date.getTime() > scope.row.receiveInvoiceDate } else { return date.getTime() < new Date().getTime() - 1 * 24 * 60 * 60 * 1000 }}"
            />
            <template v-else>
              <div>{{ scope.row.receiveInvoiceDate? parseTime(scope.row.receiveInvoiceDate,'{y}-{m}-{d}'): '-' }}</div>
            </template>
          </template>
        </template>
      </el-table-column>
      <el-table-column key="invoiceAmount" prop="invoiceAmount" label="*开票额" align="center" min-width="170" class="money-column">
        <el-table-column key="invoiceAmount" prop="invoiceAmount" label="金额" align="center" min-width="85">
          <template v-slot="scope">
          <template v-if="scope.row.type===2">
            <span>{{totalAmount?toThousand(totalAmount): totalAmount}}</span>
          </template>
          <template v-else>
            <el-input-number
              v-if="scope.row.isModify"
              v-show-thousand
              v-model.number="scope.row.invoiceAmount"
              :min="0"
              :max="props.currentRow.amount"
              :step="100"
              :precision="DP.YUAN"
              placeholder="开票额(元)"
              controls-position="right"
              @change="moneyChange(scope.row)"
            />
            <div v-else>{{ scope.row.invoiceAmount && scope.row.invoiceAmount>0? toThousand(scope.row.invoiceAmount): scope.row.invoiceAmount }}</div>
          </template>
        </template>
        </el-table-column>
        <el-table-column key="invoiceAmount1" prop="invoiceAmount" label="大写" align="center" min-width="85" :show-overflow-tooltip="true">
          <template v-slot="scope">
          <template v-if="scope.row.type===2">
            <span>{{totalAmount?'('+digitUppercase(totalAmount)+')':''}}</span>
          </template>
          <template v-else>
            <div>{{scope.row.invoiceAmount?'('+digitUppercase(scope.row.invoiceAmount)+')':''}}</div>
          </template>
        </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="invoiceType" prop="invoiceType" label="*发票类型" align="center" width="120">
        <template v-slot="scope">
          <common-select
            v-if="scope.row.isModify"
            v-model="scope.row.invoiceType"
            :options="invoiceTypeEnum.ENUM"
            type="enum"
            size="small"
            clearable
            class="filter-item"
            placeholder="发票类型"
            style="width: 100%"
            @change="invoiceTypeChange(scope.row)"
          />
          <div v-else>{{ scope.row.invoiceType? invoiceTypeEnum.VL[scope.row.invoiceType]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="110">
        <template v-slot="scope">
          <div v-if="scope.row.invoiceType !== invoiceTypeEnum.RECEIPT.V && scope.row.isModify">
            <el-input-number
              v-model="scope.row.taxRate"
              :step="1"
              :min="0"
              :max="100"
              :precision="0"
              :controls="false"
              controls-position="right"
              class="input-underline"
              style="width: 70px; text-align: center"
              placeholder="0-100"
              @change="taxMoney(scope.row)"
            />%
          </div>
          <div v-else>{{ scope.row.taxRate? scope.row.taxRate+'%': '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="branchCompanyId" prop="branchCompanyId" label="*购方单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.branchCompanyName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="supplierId" prop="supplierId" label="*销售单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.supplierName }}</div>
        </template>
      </el-table-column>
      <el-table-column prop="invoiceSerialNumber" label="*发票号码" align="center" min-width="150">
        <template v-slot="scope">
          <el-input v-if="scope.row.isModify" v-model="scope.row.invoiceSerialNumber" type="text" placeholder="发票号码" style="width: 120px" @change="checkInvoiceNo(scope.row,scope.$index)" maxlength="8"/>
          <span v-else>{{ scope.row.invoiceSerialNumber  }}</span>
        </template>
      </el-table-column>
      <el-table-column key="writtenByName" prop="writtenByName" label="办理人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.writtenByName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditUserName" prop="auditUserName" label="审核人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.auditUserName }}</div>
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
  <mForm :existInvoiceNo="invoiceNoArr" :currentRow="currentRow" :propertyType="propertyType"/>
  </div>
</template>

<script setup>
// import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'
import crudApi, { editStatus } from '@/api/contract/supplier-manage/pay-invoice/invoice'
import { ref, defineProps, watch, nextTick, provide, defineEmits } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { auditTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import { validate } from '@compos/form/use-table-validate'
import { ElMessage } from 'element-plus'
import mForm from './form'
import { contractSupplierProductPM } from '@/page-permission/contract'

const permission = contractSupplierProductPM.invoice
const emit = defineEmits(['success'])
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
  visibleValue: {
    type: Boolean,
    default: false
  },
  propertyType: {
    type: [Number, String],
    default: undefined
  }
})
const tableRef = ref()
const originRow = ref({})
const totalAmount = ref(0)
const invoiceNoArr = ref([])
provide('totalAmount', totalAmount)
const { crud, CRUD } = useCRUD(
  {
    title: '收票填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['haveInvoiceAmount', 'collectionMode', 'invoiceType', 'collectionDepositBank', 'collectionBankAccount', 'paymentBankAccount', 'paymentDepositBank', 'auditorName', 'auditTime'],
    hasPagination: true
  },
  tableRef
)

const validateTaxRate = (value, row) => {
  if (row.invoiceType !== invoiceTypeEnum.RECEIPT.V) return !!value
  return true
}

// 金额校验
const validateAmount = (value, row) => {
  if (!value) return false
  return true
}

const tableRules = {
  receiveInvoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  invoiceAmount: [{ validator: validateAmount, message: '请选择开票额', trigger: 'change', type: 'number' }],
  taxRate: [{ validator: validateTaxRate, message: '请输入税率', trigger: 'blur' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  invoiceSerialNumber: [{ required: true, message: '请输入发票号', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.pay-invoice',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.visibleValue,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

function invoiceTypeChange(row) {
  row.taxRate = undefined
}
function moneyChange(row) {
  totalAmount.value = 0
  crud.data.map(v => {
    if (v.invoiceAmount) {
      totalAmount.value += v.invoiceAmount
    }
  })
  if (totalAmount.value > props.currentRow.amount) {
    const num = row.invoiceAmount - (totalAmount.value - props.currentRow.amount)
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
  } else {
    taxMoney(row)
  }
}

function taxMoney(row) {
  if (row.invoiceAmount && row.taxRate) {
    row.tax = row.invoiceAmount * row.taxRate / 100
  }
}
function checkInvoiceNo(row) {
  if (row.invoiceSerialNumber) {
    const val = invoiceNoArr.value.find(v => v.dataIndex === row.dataIndex)
    if (invoiceNoArr.value.findIndex(v => v.invoiceSerialNumber === row.invoiceSerialNumber) > -1) {
      ElMessage({ message: '发票号已存在，请重新填写', type: 'error' })
      row.invoiceSerialNumber = undefined
      if (val) {
        val.invoiceSerialNumber = undefined
      }
    } else {
      if (val) {
        val.invoiceSerialNumber = row.invoiceSerialNumber
      } else {
        invoiceNoArr.value.push({
          invoiceSerialNumber: row.invoiceSerialNumber,
          dataIndex: row.dataIndex
        })
      }
    }
  }
}

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
  row.receiveInvoiceDate = String(row.receiveInvoiceDate)
}

async function rowDelete(row) {
  try {
    await crudApi.del(row.id)
    crud.notify(`删除成功`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
    emit('success')
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
  const rules = tableRules
  let flag = true
  row.verify = {}
  for (const rule in rules) {
    row.verify[rule] = validate(rule, rules[rule], row[rule], row)
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

CRUD.HOOK.beforeRefresh = () => {
  crud.query.orderId = props.currentRow.id
  crud.query.type = 1
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  totalAmount.value = 0
  invoiceNoArr.value = []
  data.data.content.map(v => {
    v.dataIndex = v.id + 'id'
    if (v.invoiceAmount) {
      totalAmount.value += v.invoiceAmount
    }
    if (v.invoiceSerialNumber) {
      invoiceNoArr.value.push({
        invoiceSerialNumber: String(v.invoiceSerialNumber),
        dataIndex: v.dataIndex
      })
    }
  })
  if (totalAmount.value > 0) {
    data.data.content.push({
      type: 2
    })
  }
}

CRUD.HOOK.afterAddSuccess = () => {
  emit('success')
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
    padding-left:2px;
    padding-right:5px;
  }
  ::v-deep(.el-input-number .el-input__inner, .el-input__inner) {
    text-align: left;
    padding:0 5px;
  }
  ::v-deep(.el-table .cell){
    padding-left:2px;
    padding-right:2px;
  }
}

::v-deep(.pass-tag){
  padding:0 50px;
}
</style>