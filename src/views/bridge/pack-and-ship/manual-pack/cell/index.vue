<template>
  <div class="app-container">
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
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        width="120px"
      >
        <template #default="{ row }">
          <table-cell-tag v-if="row.workshopInf" :name="row.workshopInf.name" />
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomer.name')"
        prop="monomer.name"
        :show-overflow-tooltip="true"
        label="单体"
        sortable="custom"
        align="center"
        width="100px"
      />
      <el-table-column
        v-if="columns.visible('area.name')"
        :show-overflow-tooltip="true"
        prop="area.name"
        label="区域"
        sortable="custom"
        align="center"
        width="100px"
      />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="编号"
        sortable="custom"
        align="center"
        width="120px"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        :show-overflow-tooltip="true"
        prop="specification"
        sortable="custom"
        label="规格"
        min-width="140px"
      >
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        prop="length"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`长度\n(mm)`"
        align="left"
        min-width="85px"
      >
        <template #default="{ row }">
          {{ toFixed(row.length, DP.MES_ARTIFACT_L__MM) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('material')"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="80px"
      >
        <template #default="{ row }">
          <span>{{ row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('netWeight')"
        prop="netWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单净重\n(kg)`"
        align="left"
        min-width="80px"
      >
        <template #default="{ row }">
          {{ toFixed(row.netWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('grossWeight')"
        prop="grossWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单毛重\n(kg)`"
        align="left"
        min-width="80px"
      >
        <template #default="{ row }">
          {{ toFixed(row.grossWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('drawingNumber')"
        prop="drawingNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="图号"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('quantity')"
        prop="quantity"
        sortable="custom"
        label="清单数"
        align="center"
        min-width="80px"
      />
      <el-table-column
        v-if="columns.visible('inQuantity')"
        prop="inQuantity"
        sortable="custom"
        label="入库量"
        align="center"
        min-width="80px"
      />
      <el-table-column
        v-if="columns.visible('unPackageQuantity')"
        key="unPackageQuantity"
        prop="unPackageQuantity"
        sortable="custom"
        label="可打包量"
        align="center"
        min-width="80px"
      />
      <el-table-column
        v-if="columns.visible('packageQuantity')"
        key="packageQuantity"
        prop="packageQuantity"
        sortable="custom"
        label="已打包量"
        align="center"
        min-width="80px"
      />
      <!-- <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="70px" fixed="right">
        <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`分段进行与暂停: \n
          1.无论有无生产均可以执行暂停；\n
          2.暂停后，无法打包。\n`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template #default="{row}">
          <el-tag :type="row.status === processingEnum.PROCESS.V ? 'success' : 'primary'">{{ processingEnumV[row.status].L }}</el-tag>
        </template>
      </el-table-column> -->
      <el-table-column v-permission="permission.pack" label="操作" width="70" align="center" fixed="right">
        <template #default="{ row: { sourceRow: row } }">
          <common-button type="success" icon="el-icon-plus" :disabled="ids.includes(`${row.id}`)" size="mini" @click="add(row)" />
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { getMachinePart as get } from '@/api/bridge/bridge-pack-and-ship/manual-pack'
import { computed, ref, watch, defineEmits, defineProps, defineExpose, inject } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type'
import { bridgePackTypeEnum as packTypeEnum } from '@enum-ms/bridge'
import { bridgeBoxManualPackPM as permission } from '@/page-permission/bridge'

import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import { deepClone } from '@/utils/data-type'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '手工打包（单元）',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: ['drawingNumber', 'packageQuantity'],
    queryOnPresenterCreated: false,
    hasPagination: false
  },
  tableRef
)

const packTypeK = packTypeEnum.MACHINE_PART.K
const emit = defineEmits(['add'])
const props = defineProps({
  projectId: {
    type: [String, Number],
    default: undefined
  },
  workshopId: {
    type: [String, Number],
    default: undefined
  },
  monomerId: {
    type: [String, Number],
    default: undefined
  },
  areaId: {
    type: [String, Number],
    default: undefined
  },
  maxHeight: {
    type: [String, Number],
    default: undefined
  }
})

const packData = inject('packData')
const ids = computed(() => {
  return Object.keys(packData[packTypeK])
})

watch(
  () => [props.projectId, props.workshopId, props.monomerId, props.areaId],
  () => {
    crud.toQuery()
  },
  { immediate: true, deep: true }
)

function add(row) {
  emit('add', row, packTypeK)
}

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.projectId
  crud.query.workshopId = props.workshopId
  crud.query.monomerId = props.monomerId
  crud.query.areaId = props.areaId
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.partList?.map((v, index) => {
    v.rowKey = `${packTypeK}_${Math.random()}_${index}`
    v.productQuantity = v.unPackageQuantity
    v.originNumberList = v.numberList && deepClone(v.numberList) || []
    return v
  })
}

defineExpose({
  refresh: crud.toQuery
})
</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  .cell {
    line-height: 30px;
  }
}
</style>
