<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="申购日期" align="center" />
      <el-table-column v-if="columns.visible('applicantName')" key="applicantName" prop="applicantName" show-overflow-tooltip align="center" label="申购人" />
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" show-overflow-tooltip align="center" label="申购编号" />
      <el-table-column v-if="columns.visible('materialType')" key="materialType" prop="materialType" show-overflow-tooltip align="center" label="材料分类">
        <template #default="{ row }">
          <el-tag size="medium" effect="plain">{{ row.materialType }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('reviewStatus')" key="reviewStatus" prop="reviewStatus" show-overflow-tooltip align="center" label="审核状态">
        <template #default="{ row }">
          <el-tag :type="ddReviewStatusEnum.V[row?.sourceRow?.reviewStatus].TAG" size="medium" effect="plain">{{ row.reviewStatus }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="checkPermission(permission.detail) && columns.visible('mete')" prop="mete" key="mete" label="申购量" align="center" width="120" show-overflow-tooltip>
        <template #default="{ row }">
          <udOperation
            show-detail
            :show-del="false"
            :show-edit="false"
            :data="{ id: row.id }"
          />
        </template>
      </el-table-column>
      <!--详情与审核-->
      <el-table-column v-permission="[...permission.detail, ...permission.del]" align="center" label="操作" width="120">
        <template #default="{ row }">
          <udOperation
          :data="{ id: row.id }"
            :permission="permission"
            :show-edit="false"
            :show-del="row?.sourceRow?.reviewStatus === ddReviewStatusEnum.UNREVIEWED.V"
            delPrompt="确定撤销本条数据吗？"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
    <mDetail />
  </div>
</template>

<script setup>
import crudApi from '@/api/supply-chain/requisitions-manage/requisitions'
import { ref } from 'vue'

import { scmRequisitionsPM as permission } from '@/page-permission/supply-chain'
import { ddReviewStatusEnum } from '@enum-ms/dd'
import { materialPurchaseClsEnum } from '@enum-ms/classification'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail.vue'
import udOperation from '@crud/UD.operation.vue'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()

const dataFormat = ref([
  ['reviewStatus', ['parse-enum', ddReviewStatusEnum, { f: 'L' }]],
  ['materialType', ['parse-enum', materialPurchaseClsEnum, { f: 'L' }]],
  ['createTime', 'parse-time']
])
const { crud, columns } = useCRUD(
  {
    title: '材料申购',
    permission: { ...permission },
    invisibleColumns: [],
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
</script>
<style lang="scss" scoped>
.clickable {
  width: 100%;
  cursor: pointer;
}
</style>
