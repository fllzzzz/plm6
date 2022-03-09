<template>
  <div>
    <template v-if="currentProject && currentProject.id">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="currentProject.id" :queryMonomerId="props.queryMonomerId" @currentChange="currentChange" @handleUpload="handleUpload"/>
      </div>
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
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" align="center">
        <template v-slot="scope">
          <span style="cursor: pointer;" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件" align="center">
        <template v-slot="scope">
          <span style="cursor: pointer;" @dblclick="drawingPreview(scope.row)">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" :show-overflow-tooltip="true" label="导入人" align="center"/>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" width="160px">
        <template v-slot="scope">
          <div>{{ scope.row.createTime? parseTime(scope.row.createTime,'{y}-{m}-{d}'): '-' }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        width="200px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <udOperation
            :data="scope.row"
            :show-edit="false"
          />
          <common-button size="mini" type="primary" @click="editRow(scope.row)" v-permission="permission.edit">替换</common-button>
          <!-- 下载 -->
          <e-operation :data="scope.row" :permission="permission.download" style="margin-left:5px;"/>
        </template>
      </el-table-column>
    </common-table>
      <!--分页组件-->
      <pagination />
      <uploadForm v-model="uploadVisible" :currentMonomer="currentMonomer" :globalProject="currentProject" :dataType="crud.query.dataType" @success="crud.toQuery" :currentRow="currentRow"/>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/deepen'
import { ref, watch, defineProps } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import eOperation from '@crud/E.operation'
import pagination from '@crud/Pagination'
import mHeader from './components/machine-part-header'
import { parseTime } from '@/utils/date'
import { deepenListPM } from '@/page-permission/plan'
import uploadForm from './components/upload-form'

const props = defineProps({
  currentProject: {
    type: Object,
    default: () => {}
  },
  queryMonomerId: {
    type: [Number, String],
    default: undefined
  }
})
const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}
const permission = deepenListPM.machinePart
const tableRef = ref()
const currentRow = ref({})
const uploadVisible = ref(false)
const currentMonomer = ref({})
const { crud, columns } = useCRUD(
  {
    title: '零件图纸',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['monomerId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.machinePartTable',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.currentProject,
  (val) => {
    if (val.id) {
      crud.query.projectId = val.id
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
  currentRow.value = ''
  uploadVisible.value = true
}

watch(
  () => props.queryMonomerId,
  (val) => {
    if (val) {
      crud.query.monomerId = val
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)
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
