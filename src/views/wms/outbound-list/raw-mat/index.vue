<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('userUpdateTime')"
        key="userUpdateTime"
        :show-overflow-tooltip="true"
        prop="userUpdateTime"
        label="申请日期"
        align="center"
        width="160"
        sortable="custom"
      >
        <template #default="{ row }">
          <span v-parse-time>{{ row.userUpdateTime }}</span>
        </template>
      </el-table-column>

      <el-table-column
        v-if="columns.visible('basicClass')"
        key="basicClass"
        :show-overflow-tooltip="true"
        prop="basicClass"
        label="物料种类"
        width="200"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: matClsEnum.ENUM, v: row.basicClass, bit: true }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('recipientName')"
        key="recipientName"
        :show-overflow-tooltip="true"
        prop="recipientName"
        label="领用人"
        align="center"
        width="150"
      />
      <!--编辑与删除-->
      <el-table-column v-if="checkPermission([...permission.get, ...permission.edit])" label="操作" min-width="180px" align="left">
        <template #default="{ row }">
            <common-button type="warning" icon="el-icon-s-check" size="mini" @click="toReview(row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 审核 -->
    <review v-model="reviewVisible" :data="currentRow" @refresh="crud.refresh" />
  </div>
</template>

<script setup>
import crudApi from '@/api/wms/outbound/raw-mat-outbound-list'
import { ref } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import review from './module/review.vue'

// crud交由presenter持有
const permission = {
  get: ['wms_supplier:get'],
  edit: ['wms_supplier:edit']
}

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const currentRow = ref({})
const reviewVisible = ref(false)
const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '出库清单',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

// 打开审核
function toReview(row) {
  currentRow.value = row
  reviewVisible.value = true
}
</script>
