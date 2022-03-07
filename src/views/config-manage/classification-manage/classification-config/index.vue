<template>
  <div v-loading="loading.data" class="app-container" id="hhaa">
    <div id="card-main-content" class="flex-rsc" style="padding-bottom: 20px; overflow-x: auto">
      <root-card class="box-card lv-one-card" :level="1" :data="listMap.LV1" @add="openAddDlg" @del="handleDelSuccess" />
      <child-card class="box-card lv-two-card" :level="2" :data="listMap.LV2" @add="openAddDlg" @del="handleDelSuccess" />
      <child-card class="box-card lv-three-card" :level="3" :data="listMap.LV3" @add="openAddDlg" @del="handleDelSuccess" />
    </div>
    <batchAdd v-model="visible.batchAdd" :level="addLevel" @success="handleAddSuccess" />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/classification-manage/classification-config'
import { reactive, ref, provide } from 'vue'
import { classificationEnum } from '@enum-ms/classification'
import { isNotBlank } from '@data-type/index'
import checkPermission from '@/utils/system/check-permission'
import { classConfigPM as permission } from '@/page-permission/config'

import useMaxHeight from '@compos/use-max-height'
import useRefreshStore from '@/composables/store/use-refresh-store'
import rootCard from './module/root-card.vue'
import childCard from './module/child-card.vue'
import batchAdd from './module/batch-add.vue'

// 最大高度
const { maxHeight } = useMaxHeight({ extraBox: null, wrapperBox: ['.app-container', '#card-main-content'] })

const addLevel = ref(1)
const visible = reactive({
  batchAdd: false
})
const loading = reactive({
  data: false
})
const listMap = reactive({
  LV1: [],
  LV2: [],
  LV3: []
})
const selectMap = reactive({
  current_LV1: undefined,
  current_LV2: undefined,
  current_LV3: undefined,
  current_list_LV1: undefined,
  current_list_LV2: undefined,
  current_list_LV3: undefined
})

provide('permission', permission)
provide('crudApi', crudApi)
provide('maxHeight', maxHeight)
provide('selectMap', selectMap)

fetchList()

// 打开添加窗口
function openAddDlg(level) {
  addLevel.value = level
  visible.batchAdd = true
}

async function fetchList() {
  if (!checkPermission(permission.get)) return
  loading.data = true
  try {
    // 清空老数据
    Object.keys(listMap).forEach((key) => {
      listMap[key] = []
    })
    const tree = await crudApi.get()
    // 转换数据
    tree2listByDeep(tree)
  } catch (error) {
    console.log('error', error)
  } finally {
    loading.data = false
  }
}

/**
 * 树结构数据根据层级转为多个层级的数组
 * @param {array} tree 树
 * @param {object} parent 父节点
 * @param {number} deep 树深度
 */
function tree2listByDeep(tree, parent, deep = 1) {
  const list = listMap[`LV${deep}`]
  tree.forEach((node) => {
    const n = {
      parent: parent,
      id: node.id,
      name: node.name,
      code: node.code,
      basicClass: deep === 1 ? node.basicClass : parent.basicClass,
      basicClassName: deep === 1 ? classificationEnum.VL[`${node.basicClass}`] : parent.basicClassName,
      serialNumber: `${isNotBlank(parent) ? parent.serialNumber : ''}${node.code}`
    }

    list.push(n)
    if (isNotBlank(node.children)) {
      tree2listByDeep(node.children, n, deep + 1)
    }
  })
}

function handleAddSuccess() {
  fetchList()
  useRefreshStore('classification')
}

function handleDelSuccess() {
  fetchList()
  useRefreshStore('classification')
}
</script>

<style lang="scss" scoped>
::v-deep(.card-header) {
  .el-button {
    padding: 5px;
  }
  .el-button--small {
    min-height: 25px;
  }
  .el-button + .el-button {
    margin-left: 5px;
  }
  .search-name {
    margin-top: 15px;
  }
  .search-type {
    margin-top: 15px;
    margin-left: 15px;
    // width:350px;
  }
  flex-wrap: wrap;
  .flex-rbc {
    width: 100%;
  }
}

.box-card {
  flex: 0 0 auto;
  ::-webkit-scrollbar {
    width: 6px;
    height: 6px;
  }
  ::-webkit-scrollbar-thumb {
    border-radius: 6px;
  }
}

.box-card + .box-card {
  margin-left: 10px;
}

.lv-one-card,
.lv-two-card {
  width: 420px;
  height: 100%;
}

.lv-two-card {
  width: 520px;
  height: 100%;
}

.lv-three-card {
  width: 620px;
  height: 100%;
}

.app-container {
  padding: 20px;
}
</style>
