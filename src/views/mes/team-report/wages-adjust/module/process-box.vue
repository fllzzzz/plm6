<template>
  <div>
    <template v-for="item in process" :key="item.id">
      <el-tag
        hit
        v-if="productType && productType === item.productType"
        :effect="isSingle ? (selectLineId === item.id ? 'light' : 'plain') : item.selected ? 'light' : 'plain'"
        :type="isSingle ? (selectLineId === item.id ? 'success' : 'info') : item.selected ? 'success' : 'info'"
        @click="handleChange(process, item)"
        >{{ item.name }}</el-tag
      >
    </template>
  </div>
</template>

<script setup>
import { inject, defineProps, defineEmits } from 'vue'

const emit = defineEmits(['change'])

const productType = inject('productType', undefined)

defineProps({
  process: {
    type: Array,
    default: () => []
  },
  selectLineId: {
    type: Number,
    default: undefined
  },
  isSingle: {
    type: Boolean,
    default: false
  }
})

function handleChange(process, item) {
  emit('change', { process, item })
}
</script>
