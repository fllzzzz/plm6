<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :global-project="globalProject"/>
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
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column v-if="columns.visible('serialNumber')" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="单体" width="140px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="合计数量" min-width="160px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="合计毛重(kg)" min-width="160px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('fileName')" key="fileName" prop="fileName" :show-overflow-tooltip="true" label="合计净重(kg)" min-width="160px">
        <template v-slot="scope">
          <span style="cursor: pointer;">{{ scope.row.fileName }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.detail])"
        label="操作"
        width="200px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button size="mini" type="primary" @click="openDetail(scope.row)">查看区域详情</common-button>
        </template>
      </el-table-column>
    </common-table>
      <!--分页组件-->
      <pagination />
      <mDetail :current-info="currentInfo" v-model="detailVisible"/>
    </template>
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/monomer'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mDetail from './module/detail'
// import { planTypeEnum } from '@enum-ms/plan'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['summaryList:get'],
  detail: ['summaryList:detail']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const currentInfo = ref({})
const detailVisible = ref(false)
const { crud, columns } = useCRUD(
  {
    title: '清单合计',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['productType'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.summary-list',
  paginate: true,
  extraHeight: 40
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function openDetail(row) {
  currentInfo.value = row
  detailVisible.value = true
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
