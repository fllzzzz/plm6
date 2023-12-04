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
      <el-table-column v-if="columns.visible('name')" key="name" prop="name" label="名称" align="center">
        <template #default="{row}">
          <table-cell-tag v-if="row.boolReturn" name="退量" color="#f56c6c"/>
          <span>{{ row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('specification')" key="specification" prop="specification" label="规格" align="center" />
      <el-table-column v-if="columns.visible('accountingUnit')" key="accountingUnit" prop="accountingUnit" label="核算单位" align="center" />
      <el-table-column v-if="columns.visible('mete')" key="mete" prop="mete" label="核算量" align="center" />
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
      <el-table-column label="备注" align="center" width="80" prop="remark" key="remark" :show-overflow-tooltip="true" v-if="columns.visible('remark')" />
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
            :disabled="ids.includes(`${scope.row?.id}`)"
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
import { getAuxiliaryMaterial as get } from '@/api/ship-manage/pack-and-ship/manual-pack'
import { computed, ref, watch, defineEmits, defineProps, defineExpose, inject } from 'vue'

import { artifactManualPackPM as permission } from '@/page-permission/ship-manage'
// import { DP } from '@/settings/config'
// import { toFixed } from '@data-type'
import { packTypeEnum } from '@enum-ms/mes'

import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
// import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '手工打包（辅材）',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get },
    invisibleColumns: ['drawingNumber', 'packageQuantity'],
    queryOnPresenterCreated: false,
    hasPagination: true
  },
  tableRef
)

const packTypeK = packTypeEnum.AUXILIARY_MATERIAL.K
// const packTypeK = 0
const emit = defineEmits(['add'])
const props = defineProps({
  projectId: {
    type: [String, Number],
    default: undefined
  },
  maxHeight: {
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
  }
})

const packData = inject('packData')
const ids = computed(() => {
  return Object.keys(packData[packTypeK])
})

watch(
  () => [props.projectId],
  () => {
    crud.toQuery()
  },
  { immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.projectId = props.projectId
  crud.query.monomerId = props.monomerId
  crud.query.areaId = props.areaId
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
