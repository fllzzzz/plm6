<template>
  <common-drawer ref="drawerRef" title="物流费设置" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="50%">
    <template #titleRight>
      <common-button
        v-if="crud.optShow.add"
        v-permission="permission.add"
        class="filter-item"
        size="mini"
        type="primary"
        @click.stop="crud.toAdd"
      >
        新增
      </common-button>
    </template>
    <template #content>
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
          v-if="columns.visible('project.shortName') && !crud.query.projectId"
          key="project.shortName"
          prop="project.shortName"
          :show-overflow-tooltip="true"
          label="所属项目"
          sortable="custom"
          min-width="200"
        >
          <template v-slot="scope">
            <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('supplier')"
          key="supplier"
          prop="supplier"
          :show-overflow-tooltip="true"
          label="物流公司"
          sortable="custom"
          min-width="140"
        >
          <template v-slot="scope">
            <span>{{ scope.row.supplier && scope.row.supplier?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('price')"
          key="price"
          prop="price"
          :show-overflow-tooltip="true"
          label="价格"
          sortable="custom"
          align="right"
          min-width="120"
        >
          <template v-slot="scope">
            <el-tag :type="logisticsPriceTypeEnum.V[scope.row.priceType].T" style="width: 100%" effect="plain">
              <span>{{ toFixed(scope.row.price, DP.YUAN)}}</span>
              <span style="margin-left: 3px">{{ logisticsPriceTypeEnum.V[scope.row.priceType].unit }}</span>
            </el-tag>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('tax')"
          key="tax"
          prop="tax"
          :show-overflow-tooltip="true"
          sortable="custom"
          label="税率"
          align="center"
          min-width="80"
        >
          <template v-slot="scope">
            <span v-if="scope.row.tax">{{ toFixed(scope.row.tax, DP.YUAN) }}%</span>
            <span v-else>{{ emptyTextFormatter('', '/') }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('userName')"
          key="userName"
          prop="userName"
          :show-overflow-tooltip="true"
          label="填写人"
          sortable="custom"
          align="center"
          min-width="90"
        />
        <el-table-column
          v-if="columns.visible('createTime')"
          key="createTime"
          prop="createTime"
          :show-overflow-tooltip="true"
          label="创建时间"
          sortable="custom"
          align="center"
          min-width="90"
        >
          <template v-slot="scope">
            <span v-parse-time>{{ scope.row.createTime }}</span>
          </template>
        </el-table-column>
        <!--编辑与删除-->
        <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="140px" align="center" fixed="right">
          <template v-slot="scope">
            <udOperation :data="scope.row" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm />
    </template>
  </common-drawer>
</template>

<script setup>
import { logisticsPrice as crudApi } from '@/api/mes/pack-and-ship/logistics-list'
import { defineProps, defineEmits, ref, watch } from 'vue'

import { logisticsPriceTypeEnum } from '@enum-ms/mes'
import { projectNameFormatter } from '@/utils/project'
import { DP } from '@/settings/config'
import { toFixed, emptyTextFormatter } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '物流费设置',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.head-container'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true
  },
  drawerRef
)

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      crud.toQuery()
    }
  },
  { immediate: true }
)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.projectId = v.project && v.project.id
    v.supplierId = v.supplier && v.supplier.id
    return v
  })
}
</script>
