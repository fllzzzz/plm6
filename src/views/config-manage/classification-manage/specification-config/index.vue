<template>
  <div class="app-container wrap">
    <div class="wrap-left">
      <div class="head-container">
        <el-input v-model.trim="filterText" size="small" clearable placeholder="输入科目名称、编码搜索" />
      </div>
      <div :style="heightStyle">
        <el-tree
          ref="treeMenuRef"
          v-loading="!loaded && loading.clsTree"
          :data="treeMenu"
          :props="defaultProps"
          :filter-node-method="filterDeptNode"
          style="height: 100%"
          highlight-current
          expand-on-click-node
          node-key="id"
          default-expand-all
          @node-click="handleNodeClick"
        >
          <template #default="{ node, data }">
            <div style="padding: 3px 5px; border-radius: 3px; width: 100%">
              <span style="font-weight: bold">{{ node.label }}</span>
              <span v-if="data.isLeaf" style="float: right; padding: 0 2px 0 6px">
                <span>{{ data.serialNumber }}</span>
              </span>
            </div>
          </template>
        </el-tree>
      </div>
    </div>
    <div class="wrap-right">
      <m-header id="header" ref="header" />
      <div v-loading="crud.loading" class="card-wrap">
        <el-card v-for="item in crud.data" :key="item.id" :style="heightStyle">
          <template #header>
            <div class="clearfix">
              <span style="line-height: 29px">
                {{ item.name }}
                <el-tag v-if="item.boolWeightedAverage" type="success" effect="plain" size="mini">加权</el-tag>
                <!-- <el-tag v-if="item.boolCustomizeable" style="margin-right: 4px" type="success" effect="plain" size="mini">可自定义</el-tag> -->
              </span>
              <ud-operation style="float: right" :data="item" />
            </div>
          </template>
          <div class="card-box">
            <common-table :data="item.list" :maxHeight="maxHeight - 105" row-key="id">
              <el-table-column prop="code" label="编码" width="80" align="left" />
              <el-table-column prop="value" label="规格" min-width="140" />
            </common-table>
          </div>
        </el-card>
      </div>
    </div>
    <m-form />
  </div>
</template>

<script setup>
import crudApi from '@/api/config/classification-manage/specification-config'
import { nextTick, provide, reactive, ref, watch } from 'vue'
// import { rawMatClsEnum } from '@enum-ms/classification'
import { getFirstLeafNode } from '@/utils/system/classification'
import * as lodash from 'lodash'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useMatClsTree from '@/composables/store/use-mat-cls-tree'

import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mForm from './module/form'

const permission = {
  get: ['config_specConfig:get'],
  add: ['config_specConfig:add'],
  edit: ['config_specConfig:edit'],
  del: ['config_specConfig:del'],
  weightedAverage: ['config_specConfig:weightedAverage']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const { crud } = useCRUD({
  title: '规格配置',
  optShow: { ...optShow },
  permission: { ...permission },
  requiredQuery: ['id'],
  crudApi: { ...crudApi },
  hasPagination: false
})

const { maxHeight, heightStyle } = useMaxHeight({ extraHeight: 15 })
const { loaded, rawMatClsTree } = useMatClsTree(formatTree, true)

const treeMenuRef = ref() // 菜单ref

const filterText = ref() // 菜单过滤输入
const treeMenu = ref([]) // 树菜单
// const currentRow = ref({}) // 菜单当前选中节点
const lastCurrentRow = ref({}) // 菜单当前选中节点
const defaultProps = { children: 'children', label: 'name' } // 树结构数据默认格式
const loading = reactive({
  // 加载
  clsTree: true
})

provide('currentNode', lastCurrentRow)

// tree过滤输入监听
watch(filterText, (val) => {
  treeMenuRef.value.filter(val)
})

// 拉取最新的物料分类树
async function formatTree() {
  try {
    // treeMenu.value = lodash.cloneDeep(rawMatClsTree.value.filter(v => v.basicClass !== rawMatClsEnum.SECTION_STEEL.V))
    treeMenu.value = lodash.cloneDeep(rawMatClsTree.value)
    const firstLeafNode = getFirstLeafNode(treeMenu.value)
    // 触发选中
    handleNodeClick(firstLeafNode)
    nextTick(() => {
      treeMenuRef.value.setCurrentKey(firstLeafNode.id, true)
    })
  } catch (error) {
    console.log('科目规格配置-拉取物料分类树', error)
  } finally {
    loading.clsTree = false
  }
}

// 菜单过滤
function filterDeptNode(value, data) {
  if (!value) return true
  return data.name.includes(value) || data.serialNumber.includes(value)
}

// 切换清单
function handleNodeClick(data) {
  if (data.isLeaf) {
    lastCurrentRow.value = data
    crud.query.id = data.id
    crud.toQuery()
    // 设置树节点选中
  } else {
    // currentRow.value = lastCurrentRow.value
    treeMenuRef.value.setCurrentKey(lastCurrentRow.value.id, true)
  }
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 380px;
    // margin-right: 20px;
    .el-tree {
      // width: 360px;
      overflow-y: auto;
      padding-right: 5px;
      font-size: 15px;
    }
    ::v-deep(.el-tree--highlight-current .el-tree-node.is-current > .el-tree-node__content) {
      background-color: #ffe48d !important;
    }
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}

.card-wrap {
  white-space: nowrap;
  overflow-y: auto;
  // margin-top: 12px;
  // display: flex;
  // flex-wrap: wrap;
  .el-card {
    display: inline-block;
    width: 280px;
    margin-bottom: 10px;
    margin-left: 20px;
  }
}
</style>
