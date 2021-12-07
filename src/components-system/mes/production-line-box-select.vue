<template>
  <div class="production-lines-content">
    <template v-if="!selectedAbleLineLoading">
      <template v-if="selectedAbleLineIds && selectedAbleLineIds.length > 0">
        <template v-for="factory in productLines">
          <template v-for="workshop in factory.workshopList">
            <template v-for="line in workshop.productionLineList" :key="line.id">
              <el-tooltip
                class="item"
                effect="light"
                :content="`${factory.name} - ${line.name}`"
                placement="top"
                :show-after="1000"
                v-if="selectedAbleLineIds.includes(line.id)"
              >
                <el-tag
                  hit
                  :effect="line.id == modelValue ? 'light' : 'plain'"
                  :type="line.id == modelValue ? 'success' : 'info'"
                  :disable-transitions="true"
                  size="medium"
                  @click="handleLineChange(line)"
                  >{{ line.name }}</el-tag
                >
              </el-tooltip>
            </template>
          </template>
        </template>
      </template>
      <span v-else style="font-size: 11px; color: red">{{ tip }}</span>
    </template>
    <span v-else style="font-size: 11px">生产线加载中 <i class="el-icon-loading" /></span>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, watch, defineExpose } from 'vue'

import useProductLines from '@compos/store/use-product-lines'

const emit = defineEmits(['update:modelValue', 'loaded', 'change'])
const props = defineProps({
  modelValue: {
    type: [Number, undefined],
    default: undefined
  },
  unselectable: {
    type: Boolean,
    default: false
  },
  selectedAbleLineIds: {
    type: Array,
    default: () => []
  },
  selectedAbleLineLoading: {
    type: Boolean,
    default: true
  },
  tip: {
    type: String,
    default: '* 没有含有任务的生产线'
  }
})

const { loaded, productLines } = useProductLines()

watch(
  loaded,
  (val) => {
    emit('loaded')
  },
  { immediate: true, deep: true }
)

function handleLineChange(line) {
  if (props.value !== line.id) {
    selectChange(line.id)
  } else {
    if (props.unselectable) {
      selectChange(undefined)
    }
  }
}
function getLines() {
  if (!loaded.value) {
    throw new Error('生产线未加载完成')
  }
  return productLines.value
}
function selectChange(val) {
  emit('update:modelValue', val)
  emit('change', val)
}

defineExpose({
  getLines
})
</script>

<style lang="scss" scoped>
.production-lines-content {
  display: inline-flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: center;
  flex-wrap: wrap;
  box-sizing: border-box;
  min-height: 38px;
  margin: 0;
  // padding: 10px 0;
  .el-tag {
    width: 200px;
    max-width: 300px;
    text-align: center;
    margin: 0 10px 10px 0;
    cursor: pointer;
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
  }
}
</style>
