<template>
  <div class="app-container">
    <!--表格渲染-->
    <common-button type="primary" @click="addRow">添加</common-button>
    <common-table
      ref="tableRef"
      v-loading="loading"
      :data="tableData"
      :empty-text="'暂无数据'"
      :max-height="maxHeight"
      class="table-form"
      :cell-class-name="wrongCellMask"
      style="width: 100%;margin-top:10px;"
    >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column key="collectionDate" prop="collectionDate" label="收款日期" align="center" width="150">
      <template v-slot="scope">
        <el-date-picker
          v-if="scope.row.isModify"
          v-model="scope.row.collectionDate"
          type="date"
          size="small"
          value-format="x"
          placeholder="选择日期"
          style="width:140px"
          :disabledDate="(date) => {if (scope.row.collectionDate) { return date.getTime() > scope.row.collectionDate } else { return date.getTime() < new Date().getTime() }}"
        />
        <template v-else>
          <div>{{ scope.row.collectionDate? parseTime(scope.row.collectionDate,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </template>
    </el-table-column>
    <el-table-column key="collectionAmount" prop="collectionAmount" label="*收款金额" align="center" min-width="120">
      <template v-slot="scope">
        <el-input-number
          v-if="scope.row.isModify"
          v-model.number="scope.row.collectionAmount"
          :min="-99999999999"
          :max="99999999999"
          :step="10000"
          :precision="DP.YUAN"
          placeholder="收款金额(元)"
          controls-position="right"
          style="width:100%"
        />
        <span v-else>{{ scope.row.collectionAmount && scope.row.collectionAmount>0? toThousand(scope.row.collectionAmount): scope.row.collectionAmount }}</span>
      </template>
    </el-table-column>
    <el-table-column key="collectionReason" prop="collectionReason" label="*收款事由" align="center" width="120">
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
    <el-table-column key="collectionMode" prop="collectionMode" label="*收款方式" align="center" width="120">
      <template v-slot="scope">
        <common-select
          v-if="scope.row.isModify"
          v-model="scope.row.collectionMode"
          :options="paymentFineModeEnum.ENUM"
          type="enum"
          size="small"
          placeholder="收款方式"
          style="width: 110px;"
        />
        <div v-else>{{ scope.row.collectionMode? paymentFineModeEnum.VL[scope.row.collectionMode]: '' }}</div>
      </template>
    </el-table-column>
    <el-table-column key="collectionUnit" prop="collectionUnit" label="*收款单位" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionUnit }}</div>
      </template>
    </el-table-column>
    <el-table-column key="collectionDepositBank" prop="collectionDepositBank" :show-overflow-tooltip="true" label="*收款银行" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.collectionDepositBank }}</div>
      </template>
    </el-table-column>
    <el-table-column key="paymentUnit" prop="paymentUnit" label="付款单位" align="center" min-width="120">
      <template v-slot="scope">
        <el-input
          v-if="scope.row.isModify"
          v-model="scope.row.paymentUnit"
          placeholder="付款单位"
          style="width:100%;"
        />
        <div v-else>{{ scope.row.paymentUnit  }}</div>
      </template>
    </el-table-column>
    <el-table-column key="writtenByName" prop="writtenByName" label="办理人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.writtenByName }}</div>
      </template>
    </el-table-column>
    <el-table-column key="auditorName" prop="auditorName" label="审核人" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditorName }}</div>
      </template>
    </el-table-column>
    <!-- <el-table-column v-if="columns.visible('auditStatus')" key="auditStatus" prop="auditStatus" label="状态" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.auditStatus? auditTypeEnum.VL[scope.row.auditStatus]: ''}}</div>
      </template>
    </el-table-column> -->
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([ ...permission.edit,...permission.audit])"
      label="操作"
      width="190px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <template v-if="!scope.row.isModify">
          <common-button icon="el-icon-edit" type="primary" size="mini" @click="modifyRow(scope.row)" v-if="scope.row.status===auditTypeEnum.AUDITING.V"/>
          <common-button icon="el-icon-delete" type="danger" size="mini" v-if="scope.row.status===auditTypeEnum.AUDITING.V"/>
          <common-button type="success" size="mini" v-permission="permission.audit" @click="openDetail(scope.row, 'audit')" v-if="scope.row.status===auditTypeEnum.AUDITING.V">通过</common-button>
        </template>
        <template v-else>
          <common-button type="primary" size="mini" @click="rowCancel(scope.row,scope.$index)">取消</common-button>
          <common-button type="info" plain size="mini" @click="rowSubmit(scope.row)">保存</common-button>
        </template>
        <!-- <common-button type="primary" size="mini" v-permission="permission.audit" @click="openDetail(scope.row, 'audit')" v-if="scope.row.auditStatus==auditTypeEnum.ENUM.AUDITING.V">通过</common-button> -->
      </template>
    </el-table-column>
  </common-table>
  </div>
</template>

<script setup>
// import crudApi from '@/api/contract/collection-and-invoice/collection'
import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import { auditTypeEnum } from '@enum-ms/contract'
import useDict from '@compos/store/use-dict'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { parseTime } from '@/utils/date'
import { DP } from '@/settings/config'
import { toThousand } from '@data-type/number'
import useTableValidate from '@compos/form/use-table-validate'

// crud交由presenter持有
const permission = {
  get: ['collection:get'],
  add: ['collection:add'],
  edit: ['collection:edit'],
  audit: ['collection:audit']
}

const tableRef = ref()
const loading = ref(false)
const currentInfo = ref({})
const showType = ref('detail')
const detailVisible = ref(false)
const dict = useDict(['payment_reason'])
const tableData = ref([{ id: 1, collectionDate: 1633881600000, collectionAmount: 1000, isModify: false }])
const originTableData = ref([{ id: 1, collectionDate: 1633881600000, collectionAmount: 1000, isModify: false }])
const tableRules = {
  collectionDate: [{ required: true, message: '请选择收款日期', trigger: 'change' }],
  collectionAmount: [{ required: true, message: '请选择收款金额', trigger: 'change', type: 'number' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 40
})

function openDetail(row, type) {
  currentInfo.value = row
  showType.value = type
  detailVisible.value = true
}

function addRow() {
  tableData.value.push({
    isModify: true
  })
}

function modifyRow(row) {
  row.isModify = true
  row.collectionDate = String(row.collectionDate)
}

function rowCancel(row, index) {
  row.isModify = false
  if (row.id) {
    row = Object.assign(row, originTableData.value[index])
  } else {
    tableData.value.splice(index, 1)
  }
}

function rowSubmit(row) {
  const { validResult, dealList } = tableValidate(tableData.value)
  if (validResult) {
    tableData.value = dealList
    row.isModify = false
  } else {
    return validResult
  }
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
</style>
