<template>
  <div class="app-container">
    <!--表格渲染-->
    <div>
      <common-button type="primary" size="mini" @click="crud.toAdd" style="margin-right:10px;" v-permission="permission.add">添加</common-button>
      <el-tag type="success" size="medium" v-if="contractInfo.contractAmount">{{'合同金额:'+toThousand(contractInfo.contractAmount)}}</el-tag>
      <el-tag type="success" size="medium" v-if="currentRow.settlementAmount" style="margin-left:5px;">{{'结算金额:'+toThousand(currentRow.settlementAmount)}}</el-tag>
      <print-table
        v-permission="crud.permission.print"
        api-key="collectionRecord"
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
      show-summary
      :summary-method="getSummaries"
      :cell-class-name="wrongCellMask"
      :stripe="false"
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column prop="index" label="序号" align="center" width="50" type="index" />
      <el-table-column key="collectionDate" prop="collectionDate" label="收款日期" align="center" width="160">
        <template v-slot="scope">
          <el-date-picker
            v-if="scope.row.isModify"
            v-model="scope.row.collectionDate"
            type="date"
            size="small"
            value-format="x"
            placeholder="选择日期"
            style="width:100%"
          />
          <template v-else>
            <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
          </template>
        </template>
      </el-table-column>
      <el-table-column key="collectionAmount2" prop="collectionAmount2" label="收款金额" align="center" min-width="170" class="money-column">
        <el-table-column key="collectionAmount" prop="collectionAmount" label="金额" align="center" min-width="85">
          <template v-slot="scope">
            <el-input-number
              v-if="scope.row.isModify"
              v-show-thousand
              v-model.number="scope.row.collectionAmount"
              :min="0"
              :max="999999999999"
              :step="100"
              :precision="DP.YUAN"
              placeholder="收款金额(元)"
              controls-position="right"
              :key="scope.row.dataIndex?scope.row.dataIndex:scope.row.id"
              @change="moneyChange(scope.row)"
            />
            <div v-else>{{ scope.row.collectionAmount && scope.row.collectionAmount>0? toThousand(scope.row.collectionAmount): scope.row.collectionAmount }}</div>
          </template>
        </el-table-column>
        <el-table-column key="collectionAmount1" prop="collectionAmount1" label="大写" align="center" min-width="85" :show-overflow-tooltip="true">
          <template v-slot="scope">
            <div>{{scope.row.collectionAmount?digitUppercase(scope.row.collectionAmount):''}}</div>
          </template>
        </el-table-column>
      </el-table-column>
      <el-table-column key="collectionReason" prop="collectionReason" label="收款事由" align="center" width="120">
        <template v-slot="scope">
          <common-select
            v-if="scope.row.isModify"
            v-model="scope.row.collectionReason"
            :options="dict.payment_reason"
            type="dict"
            size="small"
            clearable
            placeholder="收款事由"
            style="width:100%"
          />
          <div v-else>{{ scope.row.collectionReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ scope.row.collectionReason]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionMode" prop="collectionMode" label="收款方式" align="center" width="110">
        <template v-slot="scope">
          <common-select
            v-if="scope.row.isModify"
            v-model="scope.row.collectionMode"
            :options="paymentFineModeEnum.ENUM"
            type="enum"
            size="small"
            placeholder="收款方式"
            style="width:100%;"
          />
          <div v-else>{{ scope.row.collectionMode? paymentFineModeEnum.VL[scope.row.collectionMode]: '' }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionUnit" prop="collectionUnit" label="收款单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div>{{ scope.row.collectionUnit }}</div>
        </template>
      </el-table-column>
      <el-table-column key="collectionBankAccountId" prop="collectionBankAccountId" :show-overflow-tooltip="true" label="收款银行" align="center" min-width="120">
        <template v-slot="scope">
          <common-select
            v-if="scope.row.isModify"
            v-model="scope.row.collectionBankAccountId"
            :options="bankList"
            type="other"
            :dataStructure="typeProp"
            size="small"
            placeholder="收款银行"
            style="width:100%;"
            @change="bankChange(scope.row)"
          />
          <div v-else>{{ scope.row.collectionDepositBank }}</div>
        </template>
      </el-table-column>
      <el-table-column key="paymentUnit" prop="paymentUnit" label="付款单位" align="center" min-width="120" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <el-input
            v-if="scope.row.isModify"
            v-model.trim="scope.row.paymentUnit"
            placeholder="付款单位"
            style="width:100%;"
            maxlength="50"
          />
          <div v-else>{{ scope.row.paymentUnit  }}</div>
        </template>
      </el-table-column>
      <el-table-column key="writtenByName" prop="writtenByName" label="办理人" align="center" width="100px">
        <template v-slot="scope">
          <div>{{ scope.row.writtenByName }}</div>
        </template>
      </el-table-column>
      <el-table-column key="auditorName" prop="auditorName" label="审核人" align="center" width="100px">
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
                <common-button type="success" size="mini">通过</common-button>
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
    <mForm :projectId="projectId" :currentRow="currentRow"/>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi, { contractCollectionInfo, bankData, editStatus } from '@/api/contract/collection-and-invoice/collection'
import { ref, defineEmits, defineProps, watch, provide, nextTick } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import { tableSummary } from '@/utils/el-extra'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { auditTypeEnum } from '@enum-ms/contract'
import useDict from '@compos/store/use-dict'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { toThousand } from '@data-type/number'
import { digitUppercase } from '@/utils/data-type/number'
import { validate } from '@compos/form/use-table-validate'
import { ElMessage } from 'element-plus'
import mForm from './form'
import { contractLedgerPM } from '@/page-permission/contract'

const permission = contractLedgerPM.collection

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
const dict = useDict(['payment_reason'])
const contractInfo = ref({})
const originRow = ref({})
const bankList = ref([])
const typeProp = { key: 'id', label: 'depositBank', value: 'id' }
const totalAmount = ref(0)
const emit = defineEmits(['success'])
provide('bankList', bankList)
provide('contractInfo', contractInfo)
provide('totalAmount', totalAmount)
const { crud, CRUD } = useCRUD(
  {
    title: '收款填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['haveCollectionAmount', 'collectionMode', 'collectionReason', 'collectionDepositBank', 'collectionBankAccount', 'paymentBankAccount', 'paymentDepositBank', 'auditorName', 'auditTime'],
    hasPagination: true
  },
  tableRef
)

const tableRules = {
  collectionDate: [{ required: true, message: '请选择收款日期', trigger: 'change' }],
  collectionAmount: [{ required: true, message: '请选择收款金额', trigger: 'change', type: 'number' }],
  collectionBankAccountId: [{ required: true, message: '请选择收款银行', trigger: 'change' }],
  collectionMode: [{ required: true, message: '请选择收款方式', trigger: 'change' }],
  collectionReason: [{ required: true, message: '请选择收款事由', trigger: 'change' }],
  paymentUnit: [{ required: true, message: '请输入付款单位', trigger: 'blur' }]
}
function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
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
  () => props.projectId,
  (val) => {
    bankList.value = []
    contractInfo.value = {}
    if (val) {
      getContractInfo(val)
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

function moneyChange(row) {
  if (props.currentRow.settlementAmount) {
    totalAmount.value = 0
    crud.data.map(v => {
      if (v.collectionAmount) {
        totalAmount.value += v.collectionAmount
      }
    })
    if (totalAmount.value > props.currentRow.settlementAmount) {
      const num = row.collectionAmount - (totalAmount.value - props.currentRow.settlementAmount)
      nextTick(() => {
        row.collectionAmount = num || 0
        totalAmount.value = 0
        crud.data.map(v => {
          if (v.collectionAmount) {
            totalAmount.value += v.collectionAmount
          }
        })
      })
    }
  }
}
async function getContractInfo(id) {
  let data = {}
  try {
    data = await contractCollectionInfo({ projectId: id })
    getBankData(data.companyBankAccountList[0].companyId)
  } catch (e) {
    console.log('获取合同信息', e)
  } finally {
    contractInfo.value = data
  }
}

async function getBankData(companyId) {
  try {
    const { content } = await bankData(companyId)
    bankList.value = content
  } catch (e) {
    console.log('获取银行账号', e)
  }
}

function bankChange(row) {
  if (row.collectionBankAccountId) {
    const choseVal = bankList.value.find(v => v.id === row.collectionBankAccountId)
    row.collectionDepositBank = choseVal.depositBank
    row.collectionBankAccount = choseVal.account
  } else {
    row.collectionDepositBank = undefined
    row.collectionBankAccount = undefined
  }
}

async function passConfirm(row) {
  try {
    await editStatus(row.id, auditTypeEnum.PASS.V)
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
  row.collectionDate = String(row.collectionDate)
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
  if (row.collectionAmount === 0) {
    ElMessage.error('收款金额必须大于0')
    return
  }
  const rules = tableRules
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
    props: [['collectionAmount', DP.YUAN]],
    toThousandFields: ['collectionAmount']
  })
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.projectId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  totalAmount.value = 0
  data.data.content.map(v => {
    v.projectId = v.project.id
    if (v.collectionAmount) {
      totalAmount.value += v.collectionAmount
    }
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
