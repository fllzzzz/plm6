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
      return-source-data
      :showEmptySymbol="false"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('projectName')" key="projectName" prop="projectName" :show-overflow-tooltip="true" label="项目" min-width="100" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="合同签订时间" align="center" width="180px">
        <template v-slot="scope">
          <span>{{ scope.row.createTime?parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('signerName')" key="signerName" prop="signerName" :show-overflow-tooltip="true" label="销售负责人" min-width="160" />
       <el-table-column v-if="columns.visible('projectContent')" key="projectContent" prop="projectContent" :show-overflow-tooltip="true" label="合同内容" min-width="160" />
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" label="状态" align="center" min-width="80" />
      <el-table-column v-if="columns.visible('createName')" key="createName" prop="createName" :show-overflow-tooltip="true" label="创建人" min-width="140" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" align="center" width="180px">
        <template v-slot="scope">
          <span>{{ scope.row.createTime?parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-' }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.edit, ...permission.del])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">

        </template>
      </el-table-column>
    </common-table>
      <!--分页组件-->
      <pagination />
  </div>
</template>

<script setup>
import { allEnclosureProject as get } from '@/api/enclosure/enclosure-plan/area'
import { ref, watch } from 'vue'
import { areaListPM as permission } from '@/page-permission/plan'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'
import { manufactureTypeEnum } from '@enum-ms/plan'
import { parseTime } from '@/utils/date'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const typeInfo = ref([])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护计划',
    sort: ['sort.asc', 'id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.enclosure-area',
  paginate: true,
  extraHeight: 40
})

CRUD.HOOK.handleRefresh = (crud, data) => {
  // data.data.content = data.data.content.map(v => {
  //   v.typeTagType = v.type === manufactureTypeEnum.HOMEMADE.V ? '' : 'warning'
  //   return v
  // })
}

</script>
