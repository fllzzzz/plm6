<template>
  <div class="app-container">
    <!-- 工具栏 -->
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
      <el-table-column
        align="center"
        v-if="columns.visible('cutInstructionId')"
        key="cutInstructionId"
        prop="cutInstructionId"
        :show-overflow-tooltip="true"
        label="切割编号id"
        min-width="70"
      >
        <template v-slot="scope">
          <span>{{ scope.row.cutInstructionId }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('plateType')"
        key="plateType"
        prop="plateType"
        :show-overflow-tooltip="true"
        label="物料种类"
        min-width="70"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateType }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="70"
      >
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('thick')" key="thick" prop="thick" :show-overflow-tooltip="true" label="厚(mm)" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.thick }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('width')" key="width" prop="width" :show-overflow-tooltip="true" label="宽(mm)" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.width }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        :show-overflow-tooltip="true"
        label="长(mm)"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('uploadName')"
        key="uploadName"
        prop="uploadName"
        :show-overflow-tooltip="true"
        label="上传人"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.uploadName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('importTime')"
        key="importTime"
        prop="importTime"
        :show-overflow-tooltip="true"
        label="导入时间"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.importTime }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" label="操作" align="center">
        <template v-slot="scope">
          <common-button size="mini" type="primary" icon="el-icon-view" @click="viewDetails(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <detail :detail-data="detailObj" v-model:visible="specsVisible" />
  </div>
</template>

<script setup>
import { ref } from 'vue'
import crudApi from '@/api/cutting/project-data'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header.vue'
import pagination from '@crud/Pagination'
import detail from '../template/detail.vue'
import useMaxHeight from '@compos/use-max-height'

const specsVisible = ref(false)

const tableRef = ref()
const detailObj = ref([])
// crud交由presenter持有
const permission = {
  get: ['contractRecord:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '项目数据',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

// 查看详情
function viewDetails(row) {
  specsVisible.value = true
  detailObj.value = row
  detailObj.value.srcList = []
  detailObj.value.srcList.push(row.platePictureUrl)
  console.log(' detailObj.value', detailObj.value)
}
</script>
