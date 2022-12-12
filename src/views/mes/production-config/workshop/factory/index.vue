<template>
  <el-card>
    <template v-slot:header>
      <span style="line-height: 1.7">{{ crud.title }}列表</span>
    </template>
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      highlight-current-row
      :data-format="dataFormat"
      :max-height="maxHeight"
      :empty-text="crud.emptyText"
      style="width: 100%"
      @current-change="handleCurrentChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="工厂名称" min-width="140px" />
      <el-table-column
        v-if="columns.visible('shortName')"
        key="shortName"
        prop="shortName"
        :show-overflow-tooltip="true"
        label="工厂简称"
        min-width="140px"
      />
      <!-- <el-table-column
        v-if="columns.visible('targetProduction')"
        key="targetProduction"
        prop="targetProduction"
        :show-overflow-tooltip="true"
        label="目标产量(吨)"
        align="center"
        width="100px"
      >
        <template v-slot="{ row }">
          <span>{{ row.targetProduction }}</span>
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('tagColor')"
        key="tagColor"
        prop="tagColor"
        :show-overflow-tooltip="true"
        label="标签颜色"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span class="color-card" :style="{ 'background-color': scope.row.tagColor || TAG_FACTORY_DEF_COLOR }" />
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('sort')" key="sort" prop="sort" label="排序" align="center" width="80px" />
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="160px"
      />
    </common-table>
    <!--分页组件-->
    <pagination />
  </el-card>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/factory'
import { ref, defineEmits, inject } from 'vue'

import { configFactoryPM as permission } from '@/page-permission/config'
import { TAG_FACTORY_DEF_COLOR } from '@/settings/config'

import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

const emit = defineEmits(['click-factory'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工厂',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    invisibleColumns: ['shortName', 'remark']
  },
  tableRef
)

const dataFormat = ref([['targetProduction', ['to-fixed', 2]]])

const maxHeight = inject('maxHeight')

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  data.content.forEach((v) => {
    // v.targetProduction = (v.targetProduction && v.targetProduction / 1000) || 0
  })
}

function handleCurrentChange(val) {
  if (val) {
    emit('click-factory', val)
  }
}
</script>

<style lang="scss" scoped>
.color-card {
  display: inline-block;
  width: 50px;
  height: 20px;
  vertical-align: middle;
}
</style>
