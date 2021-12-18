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
    style="width: 100%"
  >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('templateName')" key="templateName" prop="templateName" :show-overflow-tooltip="true" label="模板名称" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.templateName }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="描述信息" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.remark }}</div>
      </template>
    </el-table-column>
    <!--编辑与删除-->
    <el-table-column
      label="操作"
      width="130px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <udOperation :data="scope.row"/>
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <mForm />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/member-config'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { auditTypeEnum, systemTypeEnum, invoiceTypeEnum } from '@enum-ms/contract'
import { DP } from '@/settings/config'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { toThousand } from '@/utils/data-type/number'

// crud交由presenter持有
const permission = {
  get: ['memberConfig:get'],
  add: ['memberConfig:add'],
  edit: ['memberConfig:edit'],
  del: ['memberConfig:del']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '项目成员模板',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.memberConfig',
  paginate: true,
  extraHeight: 157
})

CRUD.HOOK.handleRefresh = (crud,data)=>{
  // data.data.content = data.data.content.map(v => {
  //   v.projectId = v.project.id
  //   return v
  // })
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
</style>