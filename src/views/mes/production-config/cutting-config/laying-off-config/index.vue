<template>
  <el-card class="line-box box-card">
    <template v-slot:header>
      <span style="line-height: 28px">{{ crud.title }}列表</span>
      <!-- 新增 -->
      <common-button
        v-permission="permission.add"
        style="float: right; padding: 6px 10px"
        size="mini"
        type="primary"
        icon="el-icon-plus"
        @click="crud.toAdd"
      >
        新增
      </common-button>
    </template>
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      highlight-current-row
      :data-format="columnsDataFormat"
      :show-empty-symbol="false"
      :max-height="maxHeight"
      style="width: 100%"
      row-key="id"
      @current-change="handleCurrentChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('layingOffWayName')"
        header-align="center"
        align="left"
        key="layingOffWayName"
        prop="layingOffWayName"
        label="下料方式"
      >
        <template #default="{ row }">
          <table-cell-tag
            v-if="row.materialType"
            :name="materialTypeEnum.V[row.materialType].SL"
            :color="materialTypeEnum.V[row.materialType].COLOR"
            :offset="15"
          />
          <span> {{ row.layingOffWayName }} </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('taskPrefix')" :show-overflow-tooltip="true" prop="taskPrefix" label="任务前缀" align="center">
        <template #default="{ row }">
          <span> {{ row.taskPrefix }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="permission.del" label="操作" width="130px" align="center">
        <template #default="{ row }">
          <udOperation :show-edit="true" :show-del="true" :data="row" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 表单 -->
    <m-form />
  </el-card>
</template>

<script setup>
import { mesCuttingConfigPM as permission } from '@/page-permission/config'
import crudApi from '@/api/mes/production-config/unloading-config'
import { ref, inject, defineEmits } from 'vue'
import { baseTimeColumns } from '@/utils/columns-format/common'
import { materialTypeEnum } from '@enum-ms/uploading-form'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import mForm from './module/form.vue'
import mHeader from './module/header.vue'

const optShow = {
  batchAdd: false,
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()

const emit = defineEmits(['click-laying-off'])

// 表格列数据格式转换
const columnsDataFormat = ref([...baseTimeColumns])
const { crud, columns } = useCRUD(
  {
    title: '下料配置',
    hasPagination: true,
    formStore: true,
    formStoreKey: 'MES_CUTTING_CONFIG',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    dataPath: ''
  },
  tableRef
)
const maxHeight = inject('maxHeight')

function handleCurrentChange(val) {
  if (val) {
    emit('click-laying-off', val)
  }
}

</script>
