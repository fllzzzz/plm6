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
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      @row-dblclick="dbclick"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
    <el-table-column type="selection" width="55" align="center" />
    <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column key="createTime" prop="createTime" label="日期" align="center">
      <template v-slot="scope">
        <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</div>
      </template>
    </el-table-column>
    <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" :show-overflow-tooltip="true" label="导入人" align="center"/>
    <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件" align="center"/>
    <!--编辑与删除-->
    <el-table-column
      label="操作"
      width="150px"
      align="center"
      fixed="right"
    >
      <template v-slot="scope">
        <common-button size="mini" type="primary" @click="editRow(scope.row)" v-permission="permission.edit">替换</common-button>
        <!-- 下载 -->
        <e-operation :data="scope.row" :permission="permission.download" />
      </template>
    </el-table-column>
  </common-table>
  <!--分页组件-->
  <pagination />
  <uploadForm v-model="uploadVisible" :currentMonomer="currentMonomer" :globalProject="globalProject" :dataType="crud.query.dataType" @success="crud.toQuery" :currentRow="currentRow"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/deepen'
import { ref, watch } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
// import { ElNotification } from 'element-plus'
import eOperation from '@crud/E.operation'
import { parseTime } from '@/utils/date'
import { blueprintListPM as permission } from '@/page-permission/plan'
import uploadForm from './module/upload-form'

const { globalProjectId, globalProject } = mapGetters(['globalProjectId', 'globalProject'])

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const currentRow = ref({})
const currentMonomer = ref({})
const uploadVisible = ref(false)
const { crud, columns, CRUD } = useCRUD(
  {
    title: '蓝图',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId', 'type'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.bluePrint',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId.value,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId.value
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function dbclick(row, column, event) {
  // if (column.property === 'remark' && !row.edit) {
  //   row.edit = true
  // }
}

// function cancelIt(row) {
//   row.remark = row.originalRemark
//   row.edit = false
// }

// async function saveIt(row) {
//   try {
//     row.editLoading = true
//     await edit(row.id, {
//       remark: row.remark
//     })
//     ElNotification({ title: '修改成功', type: 'success', duration: 2500 })
//     row.originalRemark = row.remark
//   } catch (error) {
//     console.log('编辑备注', error)
//   } finally {
//     row.edit = false
//     row.editLoading = false
//   }
// }

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
