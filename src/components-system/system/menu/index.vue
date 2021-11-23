<template>
  <div>
    <el-input v-model.trim="filterText" size="small" clearable placeholder="输入菜单名称搜索" />
    <div style="height:240px;overflow:scroll;">
      <el-tree
        ref="treeMenuRef"
        v-loading="loading.data"
        :data="treeMenu"
        :props="defaultProps"
        :filter-node-method="filterNode"
        highlight-current
        :expand-on-click-node="false"
        node-key="id"
        accordion
        @node-click="handleNodeClick"
      >
        <template #default="{ node }">
          <div style="padding: 3px 5px; border-radius: 3px; width: 100%;">
            <span style="font-weight:bold">{{ node.label }}</span>
          </div>
        </template>
      </el-tree>
    </div>
  </div>
</template>

<script setup>
import { nextTick, defineProps, defineExpose, defineEmits, ref, reactive, watch } from 'vue'
import { menuTree } from '@/api/system/menu'
const emit = defineEmits(['selected'])
const treeMenuRef = ref()
const filterText = ref() // 过滤
const treeMenu = ref([])
const defaultProps = { children: 'children', label: 'label' }
const loading = reactive({
  // 加载
  data: false
})

const props = defineProps({
  pid: {
    type: Number,
    default: 0
  }
})
// tree过滤输入监听
watch(filterText, (val) => {
  treeMenuRef.value.filter(val)
})

watch(
  () => props.pid,
  (value) => {
    nextTick(() => {
      treeMenuRef.value.setCurrentKey(props.pid, true)
    })
  },
  { immediate: true }
)

// 菜单树
async function fetchMenuTree() {
  try {
    loading.data = true
    const menu = { id: 0, label: '顶级类目', children: [] }
    menu.children = await menuTree()
    treeMenu.value = [menu]
    nextTick(() => {
      treeMenuRef.value.setCurrentKey(props.pid, true)
    })
  } catch (error) {
    console.log('菜单树', error)
  } finally {
    loading.data = false
  }
}

// 菜单过滤
function filterNode(value, data) {
  if (!value) return true
  return data.label.includes(value)
}

// 切换清单
function handleNodeClick(data) {
  treeMenuRef.value.setCurrentKey(data.id, true)
  emit('selected', data.id)
}

reset()

function reset() {
  fetchMenuTree()
}

defineExpose({
  reset: reset
})
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  .icon-body {
    width: 100%;
    padding: 10px;
    .icon-list {
      height: 200px;
      overflow-y: scroll;
      div {
        height: 30px;
        line-height: 30px;
        margin-bottom: -5px;
        cursor: pointer;
        width: 33%;
        float: left;
      }
      span {
        display: inline-block;
        vertical-align: -0.15em;
        fill: currentColor;
        overflow: hidden;
      }
    }
  }
</style>
