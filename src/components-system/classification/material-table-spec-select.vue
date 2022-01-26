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
              :class="{ 'is-checked': opt.id === classify.id }"
              v-for="opt in filterOptions"
              :key="opt.id"
              shadow="hover"
            >
              <div class="classify-item pointer" @click="handleClassifyClick(opt)">
                <span>
                  <span class="number-badge counter-badge" v-if="opt.id && counter[opt.id]" @dblclick.stop="clearCurrentClassify(opt.id)">{{
                    counter[opt.id]
                  }}</span>
                  {{ opt.serialNumber }}
                </span>
                <span>
                  {{ opt.name }}
                  <span v-if="isNotBlank(opt.parent)" v-split="{ val: opt.parent.fullPathName, symbol: '>' }" class="parent-node-title" />
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
      :visible="props.visible"
      :classifyId="classify.id"
      :row-init-fn="props.rowInitFn"
      :mode="props.mode"
      :show-classify="false"
      :max-height="props.maxHeight"
      :expand-query="props.expandQuery"
      @accumulate-change="handleAccumulateChange"
      @select-change="handleSelectChange"
      @change="handleChange"
    />
  </div>
</template>

<script setup>
import { defineEmits, defineProps, defineExpose, ref, computed, watch, watchEffect } from 'vue'
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
    // 行初始化方法
    type: Function
  },
  classifyIds: {
    // 指定的科目范围（上级科目或当前科目）
    type: Array
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
  },
  autoSelected: {
    type: Boolean,
    default: false
  },
  expandQuery: {
    type: Boolean,
    default: false
  },
  visible: {
    type: Boolean,
    default: true
  }
})

const specRef = ref({})
const filterText = ref() // 筛选文字
const classify = ref({}) // 分类
const list = ref() // 列表
const counter = ref({})
const options = ref([]) // 选项

// 左侧列表宽度
const leftWidth = computed(() => {
  if (typeof props.tableWidth === 'number') {
    return `${props.tableWidth}px`
  }
  return props.tableWidth
})

const { matClsSpecKV } = useMatClsSpec()
const { loaded, matClsLeafList } = useMatClsList()

// 搜索过滤
const filterOptions = computed(() => {
  let list = options.value
  if (isNotBlank(filterText.value)) {
    list = list.filter((v) => {
      const sn = v.serialNumber.indexOf(filterText.value) > -1
      const fullName = v.fullPathName.some((v) => v.indexOf(filterText.value) > -1)
      return sn || fullName
    })
  }
  return list
})

watch(
  () => props.modelValue,
  (val) => {
    list.value = val
  },
  { immediate: true }
)

watchEffect(() => setOption(props.basicClass))

function setOption(basicClass) {
  if (!matClsLeafList.value) {
    options.value = []
    return
  }
  let opts
  if (basicClass) {
    opts = matClsLeafList.value.filter((v) => basicClass & v.basicClass)
  } else {
    opts = matClsLeafList.value
  }
  if (isNotBlank(props.classifyIds)) {
    opts = opts.filter((v) => {
      for (const cid of props.classifyIds) {
        if (v.fullPathId.includes(cid)) {
          return true
        }
      }
      return false
    })
  }
  if (props.autoSelected && isNotBlank(opts)) {
    setSelection(opts[0])
  } else {
    setSelection()
  }
  options.value = opts
}

// 选择物料
function handleClassifyClick(item) {
  classify.value = item
}

// 设置选中
function setSelection(item = {}) {
  classify.value = item
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
 * accumulator 模式
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
 * selector 模式
 */
function handleSelectChange(val) {
  emit('selectionChange', val)
}

// 初始化
function init() {
  specRef.value && specRef.value.init
}

// 初始化选中
function initSelected(row) {
  if (isNotBlank(row)) {
    row.forEach(({ classifyId }) => {
      const id = classifyId
      counter.value[id] = counter.value[id] ? counter.value[id] + 1 : 1
    })
  }
  specRef.value && specRef.value.initSelected(row.map((v) => v.sn))
}

// 删除
function delListItem(sn, index) {
  specRef.value && specRef.value.delListItem(sn, index)
}

// 清空
function clear() {
  specRef.value && specRef.value.clear()
}

// 根据物料分类清除
function clearByBasicClass(basicClass) {
  specRef.value && specRef.value.clearByBasicClass(basicClass)
}

defineExpose({
  delListItem,
  init,
  initSelected,
  clear,
  clearByBasicClass
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
          width: 150px;
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
