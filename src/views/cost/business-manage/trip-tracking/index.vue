<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      style="width: 100%"
      :max-height="maxHeight"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
     <el-table-column v-if="columns.visible('project.shortName')" key="project.shortName" prop="project.shortName" :show-overflow-tooltip="true" label="项目"  min-width="250" >
        <template #default="{ row }">
          <span class="project-name">{{ projectNameFormatter(row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('productType')" key="productType" prop="productType" label="装载类型" min-width="140">
        <template #default="{ row }">
          <el-tag
            v-for="item in cleanArray(EO.getBits(packTypeEnum, row.productType, 'V'))"
            style="margin-right: 5px"
            :key="item"
            :type="packTypeEnum.V[item].T"
            effect="light"
            disable-transitions
            >{{ packTypeEnum.VL[item] }}</el-tag
          >
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        label="车次"
        align="center"
        min-width="140px"
      >
        <template #default="{ row }">
          <el-tag effect="light" disable-transitions style="width: 100%; max-width: 130px">{{ row.serialNumber }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('licensePlate')" key="licensePlate" prop="licensePlate" :show-overflow-tooltip="true" label="车牌号" align="center" min-width="100" />
      <el-table-column prop="shipAmount" align="center" min-width="120" label="货物金额">
        <template #default="{ row }">
          <span v-thousand="row.shipAmount" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('auditTime')" key="auditTime" prop="auditTime" label="发运日期" align="center" min-width="120">
        <template #default="{ row }">
          <span v-parse-time="{ val: row.auditTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
      <!--详情与下载-->
      <el-table-column v-if="checkPermission([...permission.detail])" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <!-- 详情 -->
          <common-button type="primary" icon="el-icon-view" size="mini" @click.stop="showDetail(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/cost/business-manage/trip-tracking'
import { ref } from 'vue'

import { tripTrackingPM as permission } from '@/page-permission/cost'
import { packTypeEnum } from '@enum-ms/mes'
import { cleanArray } from '@/utils/data-type/array'
import EO from '@enum'
import { projectNameFormatter } from '@/utils/project'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns } = useCRUD(
  {
    title: '车次跟踪',
    sort: ['auditTime.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

const detailVisible = ref(false)
const shipInfo = ref({})

function showDetail(row) {
  shipInfo.value = row
  detailVisible.value = true
}
</script>
