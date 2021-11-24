<template>
  <div v-if="show" class="tree-item">
    <div class="tree-item-left">
      <el-checkbox
        v-if="checkable"
        v-model="current.checked"
        :indeterminate="indeterminate"
        :label="current.label"
        :disabled="disabled"
        @change="handleChange"
      />
      <span class="label-text" v-else>{{current.label}}</span>
    </div>
    <div v-if="isNotBlank(current.children)" class="tree-item-right">
      <table-item v-for="it in current.children" :key="it.key" :item="it" />
    </div>
    <div class="tree-item-clear" />
  </div>
</template>
<script>
export default {
  name: 'TableItem'
}
</script>

<script setup>
import { defineProps, ref, inject, computed } from 'vue'
import { isNotBlank, isBlank } from '@/utils/data-type'

const props = defineProps({
  item: {
    type: Object,
    default: () => ({})
  }
})

const OPT = {
  CHECKED: 1,
  SEMI_CHECKED: 2,
  UN_CHECKED: 3,
  UN_SEMI_CHECKED: 4
}

const checkedSet = inject('checkedSet')
const checkStrictly = inject('checkStrictly')
const returnLeaf = inject('returnLeaf')
const disabled = inject('disabled')
const checkable = inject('checkable')
const onlyShowChecked = inject('onlyShowChecked')
const current = ref(props.item)

// 半选状态
const indeterminate = computed(() => {
  return !current.value.checked && current.value.semiChecked
})

const show = computed(() => {
  if (onlyShowChecked) {
    return current.value.checked || current.value.semiChecked
  }
  return true
})

// 将消息传递给父节点
function handleChange(state) {
  // 是否将值加入返回的列表
  const changeable = !returnLeaf || (returnLeaf && isBlank(current.value.children))
  if (changeable) {
    if (state) {
      checkedSet.value.add(current.value.key)
    } else {
      checkedSet.value.delete(current.value.key)
    }
  }
  current.value.semiChecked = state
  const opt = state ? OPT.CHECKED : OPT.UN_CHECKED
  if (!checkStrictly) {
    // 父子节点互相关联
    setParentNode(current.value, opt)
    setChildNode(current.value, opt)
  }
}

// 当前节点选中后，设置父节点节点
function setParentNode(cur, opt) {
  let curOpt
  const pNode = cur.parent
  if (!pNode) return
  // 是否将值加入返回的列表
  const changeable = !returnLeaf || (returnLeaf && isBlank(pNode.children))
  // 子节点：选中
  if (opt === OPT.CHECKED) {
    if (!pNode.semiChecked) {
      // 节点不是“半选中状态，则设置为半选中”
      pNode.semiChecked = true
      curOpt = OPT.SEMI_CHECKED
    }
    pNode.checked = pNode.children.every((v) => v.checked) // 设置节点全选状态
    if (pNode.checked) {
      curOpt = OPT.CHECKED
      if (changeable) {
        checkedSet.value.add(pNode.key)
      }
    }
    // 节点选中或半选中状态发生变化，则向上递归
    if (curOpt && isNotBlank(pNode.parent)) {
      setParentNode(pNode, curOpt)
    }
    return
  }

  // 子节点：半选
  if (opt === OPT.SEMI_CHECKED) {
    if (!pNode.semiChecked) {
      // 节点不是“半选中状态，则设置为半选中”
      pNode.semiChecked = true
      curOpt = OPT.SEMI_CHECKED
    }
    if (curOpt && isNotBlank(pNode.parent)) {
      setParentNode(pNode, curOpt)
    }
    return
  }

  // 子节点：取消选中
  if (opt === OPT.UN_CHECKED) {
    let needUp = true
    const semiChecked = pNode.children.some((v) => v.semiChecked || v.checked) // 半选状态
    if (semiChecked === pNode.semiChecked && !pNode.checked) {
      // 如果当前节点为半选，子节点取消选中后，当前仍然为半选，则无需再向上递归
      needUp = false
    }
    pNode.semiChecked = semiChecked // 设置节点的半选状态
    if (pNode.checked) {
      pNode.checked = false
      if (changeable) {
        checkedSet.value.delete(pNode.key)
      }
    }
    if (needUp && isNotBlank(pNode.parent)) {
      setParentNode(pNode, OPT.UN_CHECKED)
    }
    return
  }
}

// 设置子节点信息
function setChildNode(cur, opt) {
  const cNode = cur.children || []
  if (opt === OPT.CHECKED) {
    cNode.forEach((n) => {
      const changeable = !returnLeaf || (returnLeaf && isBlank(n.children))
      n.semiChecked = true
      n.checked = true
      if (changeable) {
        checkedSet.value.add(n.key)
      }
      if (isNotBlank(n.children)) {
        setChildNode(n, opt)
      }
    })
  }

  if (opt === OPT.UN_CHECKED) {
    cNode.forEach((n) => {
      const changeable = !returnLeaf || (returnLeaf && isBlank(n.children))
      n.semiChecked = false
      n.checked = false
      if (changeable) {
        checkedSet.value.delete(n.key)
      }
      if (isNotBlank(n.children)) {
        setChildNode(n, opt)
      }
    })
  }
}
</script>

<style lang="scss" scoped>
.tree-item {
  border-bottom: 1px solid #e5e5e5;
  &:last-child {
    border-bottom: 0;
  }
  .label-text {
    font-size: 14px;
    height:25px;
    line-height: 25px;
    display: inline-block;
    margin: 5px 10px;
    color: #333
  }
}
.tree-item-left {
  float: left;
  width: 200px;
}
.tree-item-right {
  float: right;
  width: calc(100% - 200px);
  border-left: 1px solid #e5e5e5;
}
.tree-item-right .tree-item:last-child {
  border-bottom: 0;
}
.tree-item label {
  margin-left: 10px;
  padding: 0;
  height: 45px;
}
.tree-item-clear {
  clear: both;
  margin: 0;
  padding: 0;
  height: 0;
}
</style>
