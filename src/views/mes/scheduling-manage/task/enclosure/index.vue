<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="table"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" fixed />
      <el-table-column v-if="columns.visible('date')" prop="date" label="排产日期" align="center" width="140px">
        <template v-slot="scope">
          <div>{{ parseTime(scope.row.date, '{y}-{m}-{d}') }}</div>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('schedulingQuantity')"
        :show-overflow-tooltip="true"
        prop="schedulingQuantity"
        :label="`排产量（件/m）`"
        align="left"
      >
        <template v-slot="scope">
          <el-tag type="info" effect="plain" style="width: 95%">
            <template v-if="scope.row.schedulingQuantity">
              <span style="color: #409eff">{{ emptyTextFormatter(scope.row.schedulingQuantity) }}</span> /
              <span>{{ emptyTextFormatter(toFixed(scope.row.totalSchedulingLength, DP.MES_ENCLOSURE_L__M)) }}</span>
            </template>
            <template v-else>{{ emptyTextFormatter('') }}</template>
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        :show-overflow-tooltip="true"
        prop="taskQuantity"
        :label="`已排产量（件/m）`"
        align="left"
      >
        <template v-slot="scope">
          <el-tag type="info" effect="plain" style="width: 95%">
            <template v-if="scope.row.taskQuantity">
              <span style="color: #67c23a">{{ emptyTextFormatter(scope.row.taskQuantity) }}</span> /
              <span>{{ emptyTextFormatter(toFixed(scope.row.totalTaskLength, DP.MES_ENCLOSURE_L__M)) }}</span>
            </template>
            <template v-else>{{ emptyTextFormatter('') }}</template>
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unschedulingQuantity')"
        :show-overflow-tooltip="true"
        prop="unschedulingQuantity"
        :label="`未排产量（件/m）`"
        align="left"
      >
        <template v-slot="scope">
          <el-tag type="info" effect="plain" style="width: 95%">
            <template v-if="scope.row.unschedulingQuantity">
              <span style="color: #f56c6c">{{ emptyTextFormatter(scope.row.unschedulingQuantity) }}</span> /
              <span>{{ emptyTextFormatter(toFixed(scope.row.unschedulingLength, DP.MES_ENCLOSURE_L__M)) }}</span>
            </template>
            <template v-else>{{ emptyTextFormatter('') }}</template>
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="checkPermission([...permission.detail, ...permission.download])"
        label="操作"
        width="120px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button size="mini" type="primary" icon="el-icon-s-operation" @click="showDetail(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 详情 -->
    <common-drawer
      v-model:visible="drawerVisible"
      :load-delay="200"
      :show-delay="300"
      :title="`${parseTime(detailRow.date, '{y}年{m}月{d}日')} ：排产详情`"
      direction="rtl"
      size="100%"
      :before-close="
        () => {
          drawerVisible = false
        }
      "
    >
      <template #content>
        <m-detail :details="detailRow" @refresh="crud.toQuery"/>
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/task/machine-part'
import { ref, provide } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { parseTime } from '@/utils/date'
import { emptyTextFormatter, toFixed } from '@data-type'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mDetail from '../details'
import mHeader from './module/header'

// crud交由presenter持有
const permission = {
  get: ['taskAssignDetail:get'],
  print: ['taskAssignDetail:print'],
  detail: ['taskAssignDetail:detail'],
  download: ['taskAssignDetail:download']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

provide('needTableColumns', [
  { label: '板型', width: '120px', field: 'plate' },
  { label: '规格', width: '140px', field: 'specification' }
  // { label: `总长度\n(m)`, width: '80px', field: 'length', toFixed: true, DP: DP.MES_ENCLOSURE_L__M }
])

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '围护任务',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

const detailRow = ref({})
const drawerVisible = ref(false)

function showDetail(row) {
  detailRow.value = Object.assign({}, row)
  drawerVisible.value = true
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.productType = componentTypeEnum.ENCLOSURE.V
    v.schedulingQuantity = v.schedulingQuantity || 0
    v.taskQuantity = v.taskQuantity || 0
    v.unschedulingQuantity = v.schedulingQuantity - v.taskQuantity
    v.totalSchedulingLength = v.totalSchedulingLength || 0
    v.totalTaskLength = v.totalTaskLength || 0
    v.unschedulingLength = v.totalSchedulingLength - v.totalTaskLength
    return v
  })
}
</script>
