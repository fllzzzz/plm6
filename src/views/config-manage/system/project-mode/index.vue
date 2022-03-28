<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-button size="small" type="primary" @click="changeVisible=true" style="margin-bottom:10px;" v-permission="permission.edit" >修改项目模式</common-button>
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="项目名称" min-width="150">
        <template v-slot="scope">
          <div>{{ scope.row.name }}</div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('mode')" key="mode" prop="mode" :show-overflow-tooltip="true" label="项目模式" min-width="150">
        <template v-slot="scope">
          <div>{{ scope.row.mode? projectModeEnum.VL[scope.row.mode]: '-' }}</div>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <modeChange v-model="changeVisible" v-if="checkPermission(permission.edit)" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/system-config/project-mode'
import { ref } from 'vue'
import { projectModePM as permission } from '@/page-permission/config'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import modeChange from './module/mode-change'
import { projectModeEnum } from '@enum-ms/contract'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const changeVisible = ref(false)
const { crud, columns } = useCRUD(
  {
    title: '项目模式',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.projectMode',
  paginate: true,
  extraHeight: 60
})

</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
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
</style>
