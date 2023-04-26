<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader :project-id="globalProjectId" @currentChange="currentChange" @handleUpload="handleUpload"/>
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="[{}]"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      style="width: 100%"
    >
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="工艺类型" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件类型" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="所属项目" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件名称" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="备注" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="绑定构件数量" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="修订版本" align="center"/>
    <el-table-column key="createTime" prop="createTime" label="上传日期" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" :show-overflow-tooltip="true" label="上传人" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件" align="center"/>
    <!--编辑与删除-->
    <el-table-column
      v-if="checkPermission([...permission.edit, ...permission.download])"
      label="操作"
      width="220px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button size="mini" icon="el-icon-view" type="info" @click="detailVisible=true" />
        <common-button size="mini" icon="el-icon-edit" type="primary" @click="modifyVisible=true" v-permission="permission.edit" />
        <common-button size="mini" type="success" @click="bindVisible=true">绑定构件</common-button>
        <!-- 下载 -->
        <!-- <e-operation :data="scope.row" :permission="permission.download" /> -->
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <detail v-model="detailVisible" :currentMonomer="currentMonomer" :globalProject="globalProject" :dataType="crud.query.dataType" @success="crud.toQuery" :currentRow="currentRow"/>
  <modify-form v-model="modifyVisible" :currentMonomer="currentMonomer" :globalProject="globalProject" :dataType="crud.query.dataType" @success="crud.toQuery" :currentRow="currentRow" />
  <artifact-bind-form v-model="bindVisible" :currentMonomer="currentMonomer" :globalProject="globalProject" :dataType="crud.query.dataType" @success="crud.toQuery" :currentRow="currentRow" />
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/deepen'
import { ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import { mapGetters } from '@/store/lib'
import checkPermission from '@/utils/system/check-permission'
import { parseTime } from '@/utils/date'
import { changeFileListPM as permission } from '@/page-permission/plan'

import detail from './module/detail'
import modifyForm from './module/modify-form'
import artifactBindForm from './module/artifact-bind-form'
// import eOperation from '@crud/E.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const currentMonomer = ref({})

const modifyVisible = ref(false)
const detailVisible = ref(false)
const bindVisible = ref(false)
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工艺文件',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['dataType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.changeFile',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function editRow(row) {
  currentRow.value = row
  uploadVisible.value = true
}

function currentChange(val) {
  currentMonomer.value = val
}

function handleUpload() {
  currentRow.value = {}
  uploadVisible.value = true
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = globalProjectId.value
  return crud.query.projectId
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.edit = false
    v.originalRemark = v.remark
    v.editLoading = false
    return v
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
</style>
