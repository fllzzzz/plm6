<template>
  <el-table-column type="expand">
    <template #header>
      <el-icon class="pointer" @click="handleExpandAll"><el-icon-arrow-down v-if="expandAll" /><el-icon-arrow-right v-else /></el-icon>
    </template>
    <template #default="scope">
      <div class="table-expand-container">
        <slot :row="scope.row" />
      </div>
    </template>
  </el-table-column>
</template>

<script setup>
import { computed, defineProps, defineEmits } from 'vue'

const emit = defineEmits(['update:expandRowKeys'])

const props = defineProps({
  data: {
    type: Array,
    default: () => []
  },
  rowKey: {
    type: [Function, String]
  },
  expandRowKeys: {
    type: Array,
    default: () => []
  }
})

const expandAll = computed(() => props.data.length !== 0 && props.expandRowKeys.length === props.data.length)

// 展开所有行
function handleExpandAll() {
  if (!expandAll.value) {
    emit(
      'update:expandRowKeys',
      props.data.map((v) => v[rowKey()])
    )
  } else {
    emit('update:expandRowKeys', [])
  }
}

function rowKey(row) {
  if (typeof props.rowKey === 'function') {
    return props.rowKey(row)
  }

  if (typeof props.rowKey === 'string') {
    return props.rowKey
  }
}
</script>

<style lang="scss">
th.el-table__expand-column {
  .cell {
    line-height: 13px;
  }
}
.table-expand-container {
  p {
    color: brown;
    span {
      color: #606266;
    }
  }
}
</style>
