<template>
  <div class="material-table-spec-select flex-rss">
    <div class="left-container" :style="{ width: `${leftWidth}` }">
      <div class="filter-input-container">
        <el-input v-model="filterText" size="small" placeholder="可输入编号/名称过滤科目" />
      </div>
      <div v-if="loaded" class="classify-list" :style="{ 'max-height': `${props.maxHeight - 52}px` }">
        <template v-if="isNotBlank(options)">
          <template v-if="isNotBlank(filterOptions)">
            <el-card
              class="classify-card"
              :class="{ 'is-checked': opt.id === classifyId }"
              v-for="opt in filterOptions"
              :key="opt.id"
              shadow="hover"
            >
              <div class="classify-item pointer" @click="handleClassifyClick(opt)">
                <span>
                  <span class="number-badge counter-badge" v-if="opt.id && counter[opt.id]" @dblclick.stop="clearCurrentClassify(opt.id)">{{ counter[opt.id] }}</span>
                  {{ opt.serialNumber }}
                </span>
                <span>
                  {{ opt.name }}
                  <span v-if="isNotBlank(opt.parent)" v-arr-join="'>'" class="parent-node-title">
                    {{ opt.parent.fullNamePath }}
                  </span>
                </span>
              </div>
            </el-card>
          </template>
          <el-tag v-else class="tip-tag" type="info" size="medium" effect="plain"> 未搜索到相关科目 </el-tag>
        </template>
        <el-tag v-else class="tip-tag" type="danger" size="medium" effect="plain"> 尚未配置科目 </el-tag>
      </div>
      <el-tag v-else class="tip-tag loading-tag" size="medium" effect="plain">
        <el-icon class="is-loading"><el-icon-loading /></el-icon>
        科目加载中
      </el-tag>
    </div>
    <material-spec-select
      ref="specRef"
      class="right-container spec-select"
      v-model="list"
      :classifyId="classifyId"
      :row-init-fn="props.rowInitFn"
      :mode="props.mode"
      :show-classify="false"
      :max-height="props.maxHeight"
      @accumulate-change="handleAccumulateChange"
      @select-change="handleSelectChange"
      @change="handleChange"
    />
  </div>
</template>

<script setup>
import { defineEmits, defineProps, defineExpose, ref, computed, watch } from 'vue'
import { isNotBlank } from '@/utils/data-type'

import useMatClsSpec from '@compos/store/use-mat-cls-spec'
import materialSpecSelect from '@comp-cls/material-spec-select/index.vue'
import useMatClsList from '@/composables/store/use-mat-class-list'

const emit = defineEmits(['accumulateChange', 'selectionChange', 'change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Array,
    default: () => []
  },
  rowInitFn: {
    type: Function
  },
  mode: {
    type: String,
    default: 'accumulator' // accumulator
  },
  maxHeight: {
    type: Number,
    default: 600
  },
  tableWidth: {
    type: [Number, String],
    default: 350
  },
  basicClass: {
    type: Number
  }
})

const specRef = ref({})
const filterText = ref() // 筛选文字
const classifyId = ref() // 分类
const list = ref() // 列表
const counter = ref({})

// 左侧列表宽度
const leftWidth = computed(() => {
  if (typeof props.tableWidth === 'number') {
    return `${props.tableWidth}px`
  }
  return props.tableWidth
})

const { matClsSpecKV } = useMatClsSpec()
const { loaded, matClsList } = useMatClsList()

// 科目过滤
const options = computed(() => {
  if (props.basicClass) {
    return matClsList.value.filter((v) => props.basicClass & v.basicClass)
  }
  return matClsList.value
})

// 搜索过滤
const filterOptions = computed(() => {
  if (isNotBlank(filterText.value)) {
    return options.value.filter((v) => {
      const sn = v.serialNumber.indexOf(filterText.value) > -1
      const fullName = v.fullNamePath.some((v) => v.indexOf(filterText.value) > -1)
      return sn || fullName
    })
  }
  return options.value
})

watch(
  () => props.modelValue,
  (val) => {
    list.value = val
  },
  { immediate: true }
)

// 选择物料
function handleClassifyClick(item) {
  classifyId.value = item.id
}

// list发生变化
function handleChange(data) {
  list.value = data
  emit('update:modelValue', data)
  emit('change', data)
}

// 清空当前科目的所有规格
function clearCurrentClassify(classifyId) {
  counter.value[classifyId] = 0
  specRef.value && specRef.value.clearCurrentClassify(classifyId)
}

/**
 * selector 模式
 */
function handleAccumulateChange({ snList, addList, cancelList }) {
  if (isNotBlank(addList)) {
    addList.forEach((sn) => {
      const id = matClsSpecKV.value[sn].classify.id
      counter.value[id] = counter.value[id] ? counter.value[id] + 1 : 1
    })
  }
  if (isNotBlank(cancelList)) {
    cancelList.forEach(({ sn, num }) => {
      const id = matClsSpecKV.value[sn].classify.id
      counter.value[id] = counter.value[id] ? counter.value[id] - num : 0
    })
  }
  emit('accumulateChange', { snList, addList, cancelList })
}

/**
 * accumulator 模式
 */
function handleSelectChange(val) {
  emit('selectionChange', val)
}

// 初始化
function init() {
  specRef.value && specRef.value.init
}

// 删除
function delListItem(sn, index) {
  specRef.value && specRef.value.delListItem(sn, index)
}

// 清空
function clear() {
  specRef.value && specRef.value.clear
}

defineExpose({
  delListItem,
  init,
  clear
})
</script>

<style lang="scss" scoped>
.material-table-spec-select {
  .left-container {
    margin-right: 30px;
    flex: none;

    .filter-input-container {
      width: 100%;
      margin-bottom: 18px;
    }

    ::v-deep(.el-card__body) {
      padding: 0;
    }
    .classify-list {
      overflow: auto;
      &::-webkit-scrollbar {
        width: 0 !important;
      }

      .classify-card.is-checked {
        border-color: #e6a23c;
        color: #e6a23c;
        .classify-item {
          > span {
            &:nth-child(1) {
              border-right: 1px solid #e6a23c;
            }
          }
        }
      }
    }
    ::v-deep(.classify-list > .el-card) {
      user-select: none;
      border-color: #d3d4d6;
      &:nth-child(n) {
        margin-bottom: 10px;
      }
      &:last-child {
        margin-bottom: 0;
      }
    }
    .classify-item {
      font-size: 13px;
      height: 30px;
      line-height: 30px;

      .parent-node-title {
        float: right;
        font-size: 11px;
        margin-right: 10px;
        color: gray;
      }

      > span {
        padding: 0 10px;
        &:nth-child(1) {
          display: inline-block;
          width: 39%;
          border-right: 1px solid rgb(228, 231, 237);
        }
      }
    }
  }

  .right-container {
    flex: auto;
  }

  .counter-badge {
    background: #48c59c;
  }
}

.tip-tag {
  width: 100%;
  .is-loading {
    line-height: 5px;
  }
}
.loading-tag {
  display: inline-flex;
  align-items: center;
  .is-loading {
    margin-right: 5px;
  }
}
</style>
