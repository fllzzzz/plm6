<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      class="assembly-table"
      style="width: 100%"
      :stripe="false"
    >
      <el-table-column type="expand">
        <template #default="props">
          <div :key="`'singleTable${props.row.id}'`" style="width:850px;padding: 10px 50px;margin:0 auto;">
            <el-divider><span class="title">报销明细</span></el-divider>
            <common-table
              :key="`'singleTable${props.row.id}'`"
              :data="props.row.detailList"
              class="customer-table"
              style="width: 100%;"
              :stripe="false"
            >
              <el-table-column label="序号" type="index" align="center" width="50" />
              <el-table-column key="dictionaryLabel" prop="dictionaryLabel" label="报销种类" align="center" min-width="120">
                <template v-slot="scope">
                  <span>{{ scope.row.expenseTypeName && scope.row.dictionaryLabel? scope.row.expenseTypeName + '/' + scope.row.dictionaryLabel: scope.row.expenseTypeName }}</span>
                </template>
              </el-table-column>
              <el-table-column key="applyAmount" prop="applyAmount" label="申请金额" align="center">
                <template v-slot="scope">
                  <span>{{ scope.row.applyAmount? toThousand(scope.row.applyAmount,decimalPrecision.contract): '' }}</span>
                </template>
              </el-table-column>
              <el-table-column key="invoiceType" prop="invoiceType" label="发票类型" align="center" min-width="100">
                <template v-slot="scope">
                  <span>{{ scope.row.invoiceType ? invoiceTypeEnum.VL[scope.row.invoiceType] : '-' }}</span>
                </template>
              </el-table-column>
              <el-table-column key="invoiceNo" prop="invoiceNo" label="发票号码" align="center"/>
              <el-table-column prop="invoiceAmount" label="发票面额（元）" align="center" min-width="120">
                <template v-slot="scope">
                  <span>{{ scope.row.invoiceAmount? toThousand(scope.row.invoiceAmount,decimalPrecision.contract): '' }}</span>
                </template>
              </el-table-column>
              <el-table-column key="inputTax" prop="inputTax" label="进项税额" align="center">
                <template v-slot="scope">
                  <span>{{ scope.row.inputTax? toThousand(scope.row.inputTax,decimalPrecision.contract): '' }}</span>
                </template>
              </el-table-column>
              <el-table-column key="taxRate" prop="taxRate" label="税率" align="center" width="70">
                <template v-slot="scope">
                  <span>{{ scope.row.taxRate? scope.row.taxRate+'%': '-' }}</span>
                </template>
              </el-table-column>
            </common-table>
          </div>
        </template>
      </el-table-column>
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('projectName')"
        prop="projectName"
        key="projectName"
        :show-overflow-tooltip="true"
        align="center"
        label="项目"
      >
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applyDepartName')"
        prop="applyDepartName"
        key="applyDepartName"
        :show-overflow-tooltip="true"
        align="center"
        label="申请部门"
      >
        <template v-slot="scope">
          <span>{{ scope.row.applyDepartName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applyUserName')"
        prop="applyUserName"
        key="applyUserName"
        :show-overflow-tooltip="true"
        align="center"
        label="申请人"
      >
        <template v-slot="scope">
          <span>{{ scope.row.applyUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applyDate')"
        prop="applyDate"
        key="applyDate"
        :show-overflow-tooltip="true"
        align="center"
        label="申请日期"
      >
        <template v-slot="scope">
          <span>{{ scope.row.applyDate?parseTime(scope.row.applyDate,'{y}-{m}-{d}'):'' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('applyAmount')"
        prop="applyAmount"
        key="applyAmount"
        :show-overflow-tooltip="true"
        align="center"
        label="申请金额"
      >
        <template v-slot="scope">
          <span>{{ scope.row.applyAmount ? toThousand(scope.row.applyAmount,decimalPrecision.contract) : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('actuallyPayAmount')"
        prop="actuallyPayAmount"
        key="actuallyPayAmount"
        :show-overflow-tooltip="true"
        align="center"
        label="实付金额"
      >
        <template v-slot="scope">
          <span>{{ scope.row.actuallyPayAmount ? toThousand(scope.row.actuallyPayAmount,decimalPrecision.contract) : '' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('paymentUnit')"
        prop="paymentUnit"
        key="paymentUnit"
        :show-overflow-tooltip="true"
        align="center"
        label="付款单位"
      >
        <template v-slot="scope">
          <span>{{ scope.row.paymentUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('collectionUserName')"
        prop="collectionUserName"
        key="collectionUserName"
        :show-overflow-tooltip="true"
        align="center"
        label="收款人"
      >
        <template v-slot="scope">
          <span>{{ scope.row.collectionUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('writtenByName')"
        prop="writtenByName"
        key="writtenByName"
        :show-overflow-tooltip="true"
        align="center"
        label="填报人"
      >
        <template v-slot="scope">
          <span>{{ scope.row.writtenByName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('createTime')"
        prop="createTime"
        key="createTime"
        :show-overflow-tooltip="true"
        align="center"
        label="填报日期"
      >
        <template v-slot="scope">
          <span>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('confirmUserName')"
        prop="confirmUserName"
        key="confirmUserName"
        :show-overflow-tooltip="true"
        align="center"
        label="确认人"
      >
        <template v-slot="scope">
          <span>{{ scope.row.confirmUserName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('confirmTime')"
        prop="confirmTime"
        key="confirmTime"
        :show-overflow-tooltip="true"
        align="center"
        label="确认日期"
      >
        <template v-slot="scope">
          <span>{{ scope.row.confirmTime?parseTime(scope.row.confirmTime,'{y}-{m}-{d}'):'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('confirmStatus')"
        key="confirmStatus"
        prop="confirmStatus"
        label="状态"
        align="center"
        width="110px"
      >
        <template v-slot="scope">
          <div>{{ scope.row.confirmStatus ? reimbursementTypeEnum.VL[scope.row.confirmStatus] : '' }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('source')" key="source" prop="source" label="来源" align="center" min-width="120">
        <template v-slot="scope">
          <div>{{ scope.row.source ? systemTypeEnum.VL[scope.row.source] : '' }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission([...permission.edit,...permission.audit])" label="操作" width="260px" align="center">
        <template v-slot="scope">
          <common-button icon="el-icon-view" type="primary" size="mini" @click="openDetail(scope.row, 'detail')" />
          <common-button
            icon="el-icon-s-check"
            type="primary"
            size="mini"
            @click="openConfirm(scope.row, 'audit')"
            v-if="scope.row.confirmStatus == reimbursementTypeEnum.ENUM.AUDITING.V && checkPermission(permission.audit)"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <mDetail :collectionInfo="currentInfo" :type="showType" v-model="detailVisble" @success="crud.toQuery" />
    <detailConfirm :collectionInfo="currentInfo" :type="showType" v-model="confirmVisble" @success="crud.toQuery" />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/supplier-manage/reimbursement'
import { ref } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { reimbursementTypeEnum, systemTypeEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'
import DetailConfirm from './module/detail-confirm'

const { decimalPrecision } = useDecimalPrecision()

// crud交由presenter持有
const permission = {
  get: ['reimbursement:get'],
  add: ['reimbursement:add'],
  edit: ['reimbursement:edit'],
  audit: ['reimbursement:audit']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentInfo = ref({})
const showType = ref('detail')
const detailVisble = ref(false)
const confirmVisble = ref(false)
const { crud, columns, CRUD } = useCRUD(
  {
    title: '报销填报',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['applyDepartName', 'applyDate', 'collectionUserName', 'confirmUserName', 'confirmTime'],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.collection',
  paginate: true,
  extraHeight: 50
})

function openDetail(row, type) {
  currentInfo.value = row
  showType.value = type
  detailVisble.value = true
}

function openConfirm(row, type) {
  currentInfo.value = row
  showType.value = type
  confirmVisble.value = true
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map((v) => {
    v.detailList = []
    if (v.detailClassifyList && v.detailClassifyList.length > 0) {
      v.detailClassifyList.map((k) => {
        k.rowKey = k.expenseTypeId
        k.dictionaryLabel = k.expenseTypeName
        if (k.reimbursementDetailList && k.reimbursementDetailList.length > 0) {
          k.children = k.reimbursementDetailList
          k.applyAmount = 0
          k.invoiceAmount = 0
          k.inputTax = 0
          k.children.map((val) => {
            v.detailList.push({
              choseId: val.dictionaryId ? val.dictionaryId : val.expenseTypeId,
              applyAmount: val.applyAmount,
              dictionaryId: val.dictionaryId,
              dictionaryLabel: val.dictionaryLabel,
              expenseTypeId: val.expenseTypeId,
              expenseTypeName: k.expenseTypeName,
              id: val.id,
              inputTax: val.inputTax,
              invoiceAmount: val.invoiceAmount,
              invoiceNo: val.invoiceNo,
              invoiceType: val.invoiceType,
              taxRate: val.taxRate,
              actuallyPayAmount: val.actuallyPayAmount ? val.actuallyPayAmount : undefined
            })
            val.rowKey = `${k.expenseTypeId}__${val.id}`
            k.applyAmount += val.applyAmount
            k.invoiceAmount += val.invoiceAmount
            k.inputTax += val.inputTax
          })
        } else {
          k.children = []
          k.applyAmount = undefined
          k.invoiceAmount = undefined
          k.inputTax = undefined
        }
      })
    }
    return v
  })
}
</script>
