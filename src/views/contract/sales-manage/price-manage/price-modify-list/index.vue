<template>
  <div class="modify-container">
    <!--工具栏-->
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data-format="dataFormat"
      :data="crud.data"
      row-key="rowId"
      style="width: 100%"
      :max-height="maxHeight"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="项目" min-width="140" />
      <el-table-column v-if="columns.visible('monomer.name') && (crud.query.type!==contractSaleTypeEnum.AUXILIARY_MATERIAL.V && crud.query.type!==contractSaleTypeEnum.ENCLOSURE.V)" key="monomer.name" prop="monomer.name" label="单体" align="center" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('enclosurePlan.name') && crud.query.type===contractSaleTypeEnum.ENCLOSURE.V" key="enclosurePlan.name" prop="enclosurePlan.name" label="批次" align="center" min-width="120" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('type')" show-overflow-tooltip key="type" prop="type" align="center" label="类型" width="90" />
      <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" label="事由" align="center" min-width="140" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('createUserName')" key="createUserName" prop="createUserName" label="创建人" align="center" min-width="100" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间"  align="center"  width="130" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('checkUserName')" key="checkUserName" prop="checkUserName" label="审核人" align="center" min-width="100" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('checkTime')" key="checkTime"  prop="checkTime" label="审核时间"  align="center"  width="130" show-overflow-tooltip />
      <el-table-column v-if="columns.visible('status')" prop="status" align="center" width="90" label="状态" show-overflow-tooltip>
        <template #default="{ row }">
          <el-tag :type="reviewStatusEnum.V[row?.sourceRow?.status].TAG" size="medium" effect="plain">{{ row.status }}</el-tag>
        </template>
      </el-table-column>
      <!--详情-->
      <el-table-column v-if="checkPermission(permission.detail)" label="操作" width="70px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button icon="el-icon-view" type="info" size="mini" @click.stop="openDetail(row)" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mDetail :detail-info="detailInfo" @success="crud.refresh" />
  </div>
</template>

<script setup>
import { priceModifyGet as get, priceModifyDetail as detail } from '@/api/contract/sales-manage/price-manage/common'
import { ref, inject, watch, defineEmits } from 'vue'
import { priceManagePM as permission } from '@/page-permission/contract'

import checkPermission from '@/utils/system/check-permission'
import { contractSaleTypeEnum } from '@enum-ms/mes'
import { reviewStatusEnum } from '@enum-ms/common'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

const emit = defineEmits(['refresh-data'])
const modifyVisible = inject('modifyVisible')

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailInfo = ref({})
const dataFormat = ref([
  ['project', 'parse-project'],
  ['type', ['parse-enum', contractSaleTypeEnum, { f: 'SL' }]],
  ['status', ['parse-enum', reviewStatusEnum]],
  ['createTime', 'parse-time'],
  ['checkTime', 'parse-time']
])
const { crud, columns, CRUD } = useCRUD(
  {
    title: '商务价格变更记录',
    sort: [],
    permission: { ...permission, get: permission.list },
    crudApi: { get, detail },
    optShow: { ...optShow },
    invisibleColumns: []
  },
  tableRef
)

watch(
  modifyVisible,
  (val) => {
    if (val) {
      crud.toQuery()
    } else {
      emit('refresh-data')
    }
  },
  { immediate: true }
)

const { maxHeight } = useMaxHeight({
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body'],
  extraHeight: 20,
  paginate: true
})

CRUD.HOOK.handleRefresh = (crud) => {
  crud.data = crud.data.map((v, i) => {
    v.rowId = i + '' + Math.random()
    return v
  })
}

// 打开详情
function openDetail(row) {
  detailInfo.value = row.sourceRow
  crud.toDetail(row).sourceRow
}
</script>
