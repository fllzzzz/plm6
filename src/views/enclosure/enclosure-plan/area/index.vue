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
      :data-format="dataFormat"
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
      <el-table-column v-if="columns.visible('type')" key="type" prop="type" label="状态" align="center" min-width="80">
         <template v-slot="scope">
          <el-tag :type="enclosurePlanTypeEnum.V[scope.row.sourceRow.type].TAG" effect="plain">{{ scope.row.type }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createName')" key="createName" prop="createName" :show-overflow-tooltip="true" label="创建人" min-width="140" />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" align="center" width="180px">
        <template v-slot="scope">
          <span>{{ scope.row.createTime?parseTime(scope.row.createTime,'{y}-{m}-{d}'):'-' }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.plan.get])"
        label="操作"
        width="130px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button
            size="mini"
            icon="el-icon-plus"
            type="primary"
            v-if="!scope.row.type && checkPermission(permission.plan.get)"
            @click="openDetail(scope.row,'add')"
          />
          <common-button
            v-if="scope.row.type && checkPermission(permission.plan.get)"
            size="mini"
            icon="el-icon-view"
            type="info"
            @click="openDetail(scope.row,'detail')"
          />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <planList v-model="planVisible" :detailInfo="detailInfo" @success="crud.toQuery"/>
  </div>
</template>

<script setup>
import { allEnclosureProject as get } from '@/api/enclosure/enclosure-plan/area'
import { ref } from 'vue'

import { enclosurePlanTypeEnum } from '@enum-ms/enclosure'
import { enclosureAreaListPM as permission } from '@/page-permission/enclosure'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'

import pagination from '@crud/Pagination'
import mHeader from './module/header'
import planList from './module/plan-list/list'
import { parseTime } from '@/utils/date'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const planVisible = ref(false)
const detailInfo = ref({})

const dataFormat = ref([
  ['project', 'parse-project'],
  ['type', ['parse-enum', enclosurePlanTypeEnum]]
])

const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护计划',
    sort: ['sort.asc', 'id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: [],
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.enclosure-area',
  paginate: true,
  extraHeight: 40
})

function openDetail(row, type) {
  detailInfo.value = row
  planVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  data.data.content = data.data.content.map(v => {
    v.project = {
      id: v.projectId,
      shortName: v.shortName,
      name: v.projectName
    }
    return v
  })
}

</script>
