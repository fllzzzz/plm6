<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId"/>
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
        <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" width="140px">
        <!-- <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`双击编号可预览图纸`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>编号</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template> -->
        <template v-slot="scope">
          <span style="cursor: pointer;" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('drawingNumber')" key="drawingNumber" prop="drawingNumber" :show-overflow-tooltip="true" label="图号" width="140px" /> -->
      <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="文件" min-width="160px">
        <!-- <template slot="header"> -->
          <!-- <el-tooltip
            class="item"
            effect="light"
            :content="`双击文件名可预览图纸`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>文件</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip> -->
        <!-- </template> -->
        <template v-slot="scope">
          <span style="cursor: pointer;" @dblclick="drawingPreview(scope.row)">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" :show-overflow-tooltip="true" label="导入人" width="160px" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" width="160px">
        <template v-slot="scope">
          <div v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([ ...permission.download,...permission.del])"
        label="操作"
        width="200px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button v-if="crud.query.type === planTypeEnum.ENCLOSURE.V" size="mini" type="primary" icon="el-icon-edit" @click="editDraw(scope.row)" />
          <udOperation
            :data="scope.row"
            :show-edit="false"
          />
          <!-- 下载 -->
          <!-- <e-operation :data="scope.row" :permission="permission.download" /> -->
        </template>
      </el-table-column>
    </common-table>
      <!--分页组件-->
      <pagination />
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/technical-data-manage/deepen'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { planTypeEnum } from '@enum-ms/plan'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['deepen:get'],
  download: ['deepen:download'],
  del: ['deepen:del'],
  import: ['deepen:import']
}

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '深化图纸',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['monomerId', 'productType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.deepen',
  paginate: true,
  extraHeight: 157
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true }
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
