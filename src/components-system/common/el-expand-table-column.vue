<template>
  <el-table-column type="expand">
    <template #header>
      <el-icon class="pointer" @click="handleExpandAll"><el-arrow-down v-if="expandAll" /><el-arrow-right v-else /></el-icon>
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
      props.data.map((v) => v[props.rowKey])
    )
  } else {
    emit('update:expandRowKeys', [])
  }
}
</script>
