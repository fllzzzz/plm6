<template>
  <div class="app-container" style="padding:0;">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :currentProjectType="currentProjectType"/>
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
    <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="合同编号" min-width="250">
      <template v-slot="scope">
        <span>{{ scope.row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('name')" key="name" prop="name" :show-overflow-tooltip="true" label="项目" min-width="250">
      <template v-slot="scope">
        <span class="project-name">{{ scope.row.name }}</span>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('businessType')" key="businessType" prop="businessType" label="业务类型" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.businessType? businessTypeEnum.VL[scope.row.businessType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectType')" key="projectType" prop="projectType" label="项目类型" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectType? projectTypeEnumN.VL[scope.row.projectType]: '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="项目内容" align="center" min-width="120">
      <template v-slot="scope">
        <div>{{ scope.row.projectContent }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('attachmentCount')" key="attachmentCount" prop="attachmentCount" label="文件份数" align="center" width="110px">
      <template v-slot="scope">
        <div>{{ scope.row.attachmentCount }}</div>
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
        <!-- <udOperation
          :data="scope.row"
          :show-edit="false"
        /> -->
        <!-- 下载 -->
        <!-- <e-operation :data="scope.row" :permission="permission.download" /> -->
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/contract/change-audit-log'
import { ref, watch, defineProps } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { projectTypeEnumN, businessTypeEnum } from '@enum-ms/contract'

const { globalProjectId, currentProjectType } = mapGetters(['globalProjectId', 'currentProjectType'])
// crud交由presenter持有
const permission = {
  get: ['changeAuditLog:get'],
  editStatus: ['changeAuditLog:editStatus']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '变更管理',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  }
)

const props=defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

const { maxHeight } = useMaxHeight({
  wrapperBox: '.changeAuditLog',
  paginate: true,
  extraHeight: 157
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.projectId
  return !!crud.query.projectId
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
