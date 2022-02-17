<template>
  <el-table-column v-if="showProject" key="project" prop="project" label="项目" align="left" min-width="120px" show-overflow-tooltip>
    <template #default="{ row }">
      <table-cell-tag v-if="showTransfer && row.boolTransfer" name="调拨" type="transfer" :offset="15" />
      <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
    </template>
  </el-table-column>
  <el-table-column v-if="showWarehouse" key="warehouse" prop="warehouse" label="仓库" align="left" min-width="110px" show-overflow-tooltip>
    <template #default="{ row }">
      <factory-table-cell-tag v-if="props.showFactory" :id="row.factory ? row.factory.id : row.factoryId" />
      <span v-empty-text>{{ row.warehouse ? row.warehouse.name : row.warehouseName }}</span>
    </template>
  </el-table-column>
</template>

<script setup>
import { defineProps, computed } from 'vue'
import { isBlank } from '@/utils/data-type'
import factoryTableCellTag from '@comp-base/factory-table-cell-tag.vue'

const props = defineProps({
  showProject: { // 显示项目
    type: Boolean,
    default: false
  },
  showTransfer: { // 项目标签 显示调拨
    type: Boolean,
    default: false
  },
  showFactory: { // 显示工厂
    type: Boolean,
    default: true
  },
  columns: { // 用于crud组件的列显隐
    type: Object
  }
})

const showWarehouse = computed(() => isBlank(props.columns) || props.columns.visible('warehouse'))
const showProject = computed(() => props.showProject && (isBlank(props.columns) || props.columns.visible('project')))

</script>

