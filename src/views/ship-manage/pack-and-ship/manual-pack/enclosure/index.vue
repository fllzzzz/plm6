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
        <template v-slot="scope">
          <table-cell-tag v-if="scope.row.workshop" :name="scope.row.workshop.name" />
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('monomer.name')"
        prop="monomer.name"
        label="单体"
        sortable="custom"
        align="center"
        width="100px"
      />
      <el-table-column v-if="columns.visible('area.name')" prop="area.name" label="区域" sortable="custom" align="center" width="100px" /> -->
      <el-table-column
        v-if="columns.visible('serialNumber')"
        prop="serialNumber"
        label="编号"
        sortable="custom"
        align="center"
        width="120px"
      />
      <el-table-column
        v-if="columns.visible('plate')"
        key="plate"
        prop="plate"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="板型"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('color')"
        key="color"
        prop="color"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="颜色"
        align="center"
        min-width="100px"
      />
      <el-table-column
        v-if="columns.visible('weight')"
        key="weight"
        prop="weight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单重\n(kg)`"
        align="center"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ scope.row.weight }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('surfaceArea')"
        key="surfaceArea"
        prop="surfaceArea"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单面积\n(m²)`"
        align="center"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ convertUnits(scope.row.surfaceArea, 'mm²', 'm²', DP.COM_AREA__M2) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="单长（mm）"
        align="center"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('totalArea')"
        key="totalArea"
        prop="totalArea"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`总面积\n(㎡)`"
        align="left"
        min-width="100px"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.totalArea, DP.COM_AREA__M2) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('drawingNumber')"
        key="drawingNumber"
        prop="drawingNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="图号"
        min-width="140px"
      /> -->
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        label="清单数（件）"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('inQuantity')"
        key="inQuantity"
        prop="inQuantity"
        sortable="custom"
        label="入库数（件）"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('unPackageQuantity')"
        key="unPackageQuantity"
        prop="unPackageQuantity"
        sortable="custom"
        label="可打包数（件）"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('packageQuantity')"
        key="packageQuantity"
        prop="packageQuantity"
        sortable="custom"
        label="已打包数（件）"
        align="center"
      />
      <!-- <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="70px" fixed="right">
        <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`构件进行与暂停: \n
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
        <template v-slot="scope">
          <el-tag :type="scope.row.status === processingEnum.PROCESS.V ? 'success' : 'primary'">{{ processingEnumV[scope.row.status].L }}</el-tag>
        </template>
      </el-table-column> -->
      <el-table-column v-permission="permission.pack" label="操作" width="70" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            type="success"
            icon="el-icon-plus"
            :disabled="ids.includes(`${scope.row.id}`)"
            size="mini"
            @click="add(scope.row)"
          />
        </template>
      </el-table-column>
    </common-table>
    <!-- 分页 -->
    <pagination />
  </div>
</template>

<script setup>
import { getEnclosure as get } from '@/api/ship-manage/pack-and-ship/manual-pack'
import { computed, ref, watch, defineEmits, defineProps, defineExpose, inject } from 'vue'

import { artifactManualPackPM as permission } from '@/page-permission/ship-manage'
import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { toFixed } from '@data-type'
import { packTypeEnum } from '@enum-ms/mes'

import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'
import pagination from '@crud/Pagination'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '手工打包（围护）',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: ['color', 'packageQuantity'],
    queryOnPresenterCreated: false,
    hasPagination: true
  },
  tableRef
)

const packTypeK = packTypeEnum.ENCLOSURE.K
// const packTypeK = 0
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
  () => [props.projectId, props.workshopId, props.batchId],
  () => {
    setTimeout(() => {
      crud.toQuery()
    }, 100)
  },
  { immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.projectId
  crud.query.workshopId = props.workshopId
  crud.query.areaId = props.areaId ? props.areaId : undefined
}

function add(row) {
  emit('add', row, packTypeK)
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content?.map((v) => {
    v.productQuantity = v.unPackageQuantity
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
