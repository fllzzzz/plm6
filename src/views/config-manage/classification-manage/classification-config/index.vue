<template>
  <div v-loading="loading.data" class="app-container">
    <div class="flex-rsc" style="padding-bottom: 20px;;overflow-x: auto;">
      <rootCard class="box-card lv-one-card" title="一级科目" :data="listMap.LV_1"/>

      <div>
        <!-- <batchAdd v-model:visible="batchAddVisible" :default-level="defaultAdd_LV" @success="handleAddSuccess" /> -->
      </div>
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/classification-manage/classification-config'
import rootCard from './module/root-card.vue'
import { reactive, provide } from 'vue'
import { isNotBlank } from '@/utils/data-type'

import useCheckPermission from '@compos/use-check-permission'

// 权限
const permission = {
  get: ['config_wms_classConfig:get'],
  add: ['config_wms_classConfig:add'],
  del: ['config_wms_classConfig:del']
}

provide('permission', permission)
provide('crudApi', crudApi)

const loading = reactive({
  data: false
})
const listMap = reactive({
  LV_1: [], LV_2: [], LV_3: []
})

fetchList()

async function fetchList() {
  if (!useCheckPermission(permission.get)) return
  loading.data = true
  try {
    const { content = [] } = await crudApi.get()
    // 转换数据
    tree2listByDeep(content)
  } catch (error) {
    Object.assign(listMap).forEach(key => {
      listMap[key] = []
    })
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
  const list = listMap[`LV_${deep}`]
  tree.forEach(node => {
    const n = {
      parent: parent,
      id: node.id,
      name: node.name,
      code: node.code,
      serialNumber: `${parent.code || ''}${node.code}`
    }
    list.push(n)
    if (isNotBlank(node.children)) {
      tree2listByDeep(node.children, n, deep + 1)
    }
  })
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
  .el-button+.el-button {
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
  .flex-rbc{
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

.box-card+.box-card{
    margin-left: 10px;
}

.lv-one-card, .lv-two-card {
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

</style>
