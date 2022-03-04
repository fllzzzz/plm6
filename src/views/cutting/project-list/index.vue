<template>
  <div class="app-container">
    <!-- 工具栏 -->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight - 50"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        align="center"
        v-if="columns.visible('projectName')"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="项目名称"
        min-width="100"
      >
        <template v-slot="scope">
          <span>
            {{ scope.row.projectName }}
          </span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('plateNo')"
        key="plateNo"
        prop="plateNo"
        :show-overflow-tooltip="true"
        label="钢板编号"
        min-width="70"
      >
        <template v-slot="scope">
          <span>{{ scope.row.plateNo }}</span>
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
      <el-table-column v-if="columns.visible('brand')" key="brand" prop="brand" :show-overflow-tooltip="true" label="品牌" min-width="60">
        <template v-slot="scope">
          <span>{{ scope.row.brand }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('furnaceNo')"
        key="furnaceNo"
        prop="furnaceNo"
        :show-overflow-tooltip="true"
        label="炉批号"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.furnaceNo }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('state')"
        key="state"
        prop="state"
        align="center"
        :show-overflow-tooltip="true"
        label="状态"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ steelPlateEnum.VL[scope.row.plateState] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        align="center"
        v-if="columns.visible('allocateTime')"
        key="allocateTime"
        prop="allocateTime"
        :show-overflow-tooltip="true"
        label="分配时间"
        min-width="100"
      >
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.allocateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('machineName')"
        key="machineName"
        prop="machineName"
        :show-overflow-tooltip="true"
        label="关联机器"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.machineName }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.updateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
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
import mHeader from './module/header'
import { parseTime } from '@/utils/date'
import { steelPlateEnum } from '@enum-ms/cutting'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'
import detail from '../template/detail.vue'

const tableRef = ref()
const detailObj = ref([])
const specsVisible = ref(false)

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
    title: '项目清单',
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
}

</script>
