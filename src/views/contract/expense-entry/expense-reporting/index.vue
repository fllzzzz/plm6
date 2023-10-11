<template>
  <div class="app-container">
    <mHeader />
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :data-format="columnsDataFormat"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" :selectable="(row) => !row.isAmortization" />
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('reimburseDate')"
        align="center"
        key="reimburseDate"
        prop="reimburseDate"
        :show-overflow-tooltip="true"
        label="报销日期"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('deptName')"
        align="center"
        key="deptName"
        prop="deptName"
        :show-overflow-tooltip="true"
        label="部门"
      />
      <el-table-column
        v-if="columns.visible('reimburseUserName')"
        align="center"
        key="reimburseUserName"
        prop="reimburseUserName"
        :show-overflow-tooltip="true"
        label="报销人"
      />
      <el-table-column
        v-if="columns.visible('costAscriptionEnum')"
        align="center"
        key="costAscriptionEnum"
        prop="costAscriptionEnum"
        :show-overflow-tooltip="true"
        label="费用归属"
      />
      <el-table-column
        v-if="columns.visible('expenseTypeName')"
        align="center"
        key="expenseTypeName"
        prop="expenseTypeName"
        :show-overflow-tooltip="true"
        label="费用类别"
      />
      <el-table-column
        v-if="columns.visible('expenseSubjectName')"
        align="center"
        key="expenseSubjectName"
        prop="expenseSubjectName"
        :show-overflow-tooltip="true"
        label="费用科目"
      />
      <el-table-column
        v-if="columns.visible('reimburseAmount')"
        align="right"
        key="reimburseAmount"
        prop="reimburseAmount"
        :show-overflow-tooltip="true"
        label="报销金额（元）"
      >
        <template v-slot="scope">
          <span>{{ scope.row.reimburseAmount?.toFixed(decimalPrecision.contract) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('project')"
        align="center"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="160"
      />
      <el-table-column
        v-if="columns.visible('writtenByName')"
        align="center"
        key="writtenByName"
        prop="writtenByName"
        :show-overflow-tooltip="true"
        label="填报人"
      />
      <el-table-column
        v-if="columns.visible('payee')"
        align="center"
        key="payee"
        prop="payee"
        :show-overflow-tooltip="true"
        label="收款单位"
      />
      <el-table-column
        v-if="columns.visible('remark')"
        align="center"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
      />
      <el-table-column
        v-if="columns.visible('approvalUserName')"
        align="center"
        key="approvalUserName"
        prop="approvalUserName"
        :show-overflow-tooltip="true"
        label="审核人"
      />
      <el-table-column
        v-if="columns.visible('approvalDate')"
        align="center"
        key="approvalDate"
        prop="approvalDate"
        :show-overflow-tooltip="true"
        label="审核日期"
        width="100"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        align="center"
        key="createTime"
        prop="createTime"
        :show-overflow-tooltip="true"
        label="创建时间"
        width="150"
      />
      <el-table-column
        v-if="columns.visible('approvalStatus')"
        align="center"
        fixed="right"
        key="approvalStatus"
        prop="approvalStatus"
        :show-overflow-tooltip="true"
        label="状态"
        width="90"
      >
        <template #default="{ row }">
          <el-tag :type="reviewStatusEnum.V[row.sourceRow?.approvalStatus].TAG" size="medium" effect="plain">{{
            row.approvalStatus
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column fixed="right" align="left" label="操作" width="168">
        <template v-slot="{ row }">
          <el-tag v-if="row.isAmortization" size="medium" type="success" effect="plain"> 已摊销 </el-tag>
          <div v-else>
            <common-button
              v-if="row.sourceRow?.approvalStatus === reviewStatusEnum.UNREVIEWED.V"
              v-permission="permission.audit"
              size="mini"
              type="warning"
              icon="el-icon-s-check"
              @click="openDetail(row)"
            />
            <common-button v-else size="mini" type="info" icon="el-icon-view" @click="openDetail(row)" />
            <udOperation :data="row" :show-edit="row.sourceRow?.approvalStatus === reviewStatusEnum.UNREVIEWED.V" />
          </div>
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
    <!-- 表单 -->
    <m-form />
    <!-- 详情 -->
    <m-detail v-model="detailVisible" :detail="rowDetail" @success="crud.toQuery" />
  </div>
</template>
<script setup>
import crudApi, { getExpenseType } from '@/api/contract/expense-entry/expense-reporting'
import { ref, provide } from 'vue'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { expenseReportingPM as permission } from '@/page-permission/contract'
import { costAscriptionEnum } from '@enum-ms/config'
import { reviewStatusEnum } from '@/utils/enum/modules/common'

import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header.vue'
import mForm from './module/form.vue'
import mDetail from './module/detail.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const optShow = {
  add: true,
  edit: false,
  del: true,
  download: false
}
const tableRef = ref()
const expenseList = ref([])
const cascaderTree = ref([])
const detailVisible = ref(false)
const rowDetail = ref()

const columnsDataFormat = ref([
  ['reimburseAmount', 'to-thousand'],
  ['createTime', ['parse-time', '{y}-{m}-{d} {h}:{i}:{s}']],
  ['reimburseDate', ['parse-time', '{y}-{m}-{d}']],
  ['approvalDate', ['parse-time', '{y}-{m}-{d}']],
  ['project', 'parse-project'],
  ['costAscriptionEnum', ['parse-enum', costAscriptionEnum]],
  ['approvalStatus', ['parse-enum', reviewStatusEnum]]
])

provide('expenseList', expenseList)
provide('cascaderTree', cascaderTree)

const { crud, columns } = useCRUD(
  {
    title: '费用填报',
    sort: [],
    optShow: { ...optShow },
    permission: { ...permission },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  paginate: true
})

initExpenseType()

async function initExpenseType() {
  try {
    const { content = [] } = await getExpenseType()
    expenseList.value = content
    const enumKV = costAscriptionEnum.V
    expenseList.value.forEach((row) => {
      const _row = {
        ...row
      }
      if (enumKV[row.costAscriptionEnum]?.links) {
        enumKV[row.costAscriptionEnum].links.push(_row)
      } else {
        enumKV[row.costAscriptionEnum].links = [_row]
      }
    })
    const tree = []
    for (const key in enumKV) {
      const value = enumKV[key]
      tree.push({
        id: value.V,
        name: value.L,
        links: value.links
      })
    }
    cascaderTree.value = tree
  } catch (e) {
    console.log('获取费用类别失败', e)
  }
}

function openDetail(row) {
  detailVisible.value = true
  rowDetail.value = row
}
</script>
