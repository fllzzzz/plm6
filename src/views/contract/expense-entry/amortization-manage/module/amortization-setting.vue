<template>
  <common-drawer
    customClass="amortization-setting-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    title="摊销设置"
    :wrapper-closable="true"
    size="50%"
  >
    <template #content>
      <div class="flex-r">
        <el-card body-style="padding: 10px" class="amortization-tree">
          <template #header>
            <div>
              <span>材料-摊销分类设置</span>
              <div class="icons">
                <template v-if="materialEdit">
                  <svg-icon
                    v-loading="materialEditLoading"
                    class="icon"
                    icon-class="comp-save"
                    style="fill: #ffac00"
                    @click="saveMaterial"
                  />
                  <svg-icon v-loading="materialEditLoading" class="icon" icon-class="comp-quit" @click="materialEdit = false" />
                </template>
                <el-edit v-else class="icon" @click="edit(true)" />
              </div>
            </div>
          </template>
          <el-tree
            ref="materialTreeRef"
            v-loading="materialLoading"
            :style="{ maxHeight: maxHeight + 'px' }"
            :data="materialEdit ? materialTree : amortizationKV[amortizationTypeEnum.MATERIAL.V]?.children || []"
            :props="defaultProps"
            :show-checkbox="materialEdit"
            :expand-on-click-node="false"
            node-key="id"
            highlight-current
            default-expand-all
          />
        </el-card>
        <el-card body-style="padding: 10px" class="other-tree">
          <template #header>
            <span>间接费用-摊销分类设置</span>
            <div class="icons">
              <template v-if="otherEdit">
                <svg-icon v-loading="otherEditLoading" class="icon" icon-class="comp-save" style="fill: #ffac00" @click="saveOther" />
                <svg-icon v-loading="otherEditLoading" class="icon" icon-class="comp-quit" @click="otherEdit = false" />
              </template>
              <el-edit v-else class="icon" @click="otherEdit = true" />
            </div>
          </template>
          <el-tree
            ref="otherTreeRef"
            v-loading="otherLoading"
            :style="{ maxHeight: maxHeight + 'px' }"
            :data="otherEdit ? otherTree : amortizationKV[amortizationTypeEnum.OTHER_EXPENSES.V]?.children || []"
            :props="defaultProps"
            :expand-on-click-node="false"
            node-key="id"
            highlight-current
            default-expand-all
          />
        </el-card>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { amortizationClassTree } from '@/api/contract/expense-entry/amortization-manage'
import { saveAmortizationClass } from '@/api/contract/expense-entry/amortization-manage'
import { ref, defineEmits, defineProps, inject } from 'vue'

import { amortizationTypeEnum } from '@enum-ms/contract'
import { getChildIds } from '@/utils/data-type/tree'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const amortizationKV = inject('amortizationKV')

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const materialTreeRef = ref()
const materialTree = ref([])
const materialEdit = ref(false)
const materialLoading = ref(false)
const materialEditLoading = ref(false)
const otherTreeRef = ref()
const otherTree = ref([])
const otherEdit = ref(false)
const otherLoading = ref(false)
const otherEditLoading = ref(false)
const defaultProps = ref({
  children: 'children',
  label: 'name'
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.amortization-setting-drawer',
    extraBox: ['.el-drawer__header', '.el-card__header'],
    wrapperBox: ['.el-drawer__body'],
    extraHeight: 22
  },
  visible
)

// 编辑
function edit(val) {
  if (val) {
    materialEdit.value = true
    getMaterialTree()
  }
}

// 获取材料分类的全部数据
async function getMaterialTree() {
  try {
    materialLoading.value = true
    const data = await amortizationClassTree({ amortizationClassEnum: amortizationTypeEnum.MATERIAL.V })
    materialTree.value = data?.[0]?.children || []
    materialTreeRef.value.setCheckedKeys(getChildIds(amortizationKV.value[amortizationTypeEnum.MATERIAL.V]?.children))
  } catch (error) {
    console.log('获取材料树失败', error)
  } finally {
    materialLoading.value = false
  }
}

// 保存材料分类
async function saveMaterial() {
  try {
    materialEditLoading.value = true
    const ids = materialTreeRef.value.getCheckedNodes(false, true).map((row) => row.id)
    await saveAmortizationClass({
      amortizationClassEnum: amortizationTypeEnum.MATERIAL.V,
      ids
    })
    emit('success')
    materialTree.value = filterTree(JSON.parse(JSON.stringify(materialTree.value)), ids)
    materialEdit.value = false
  } catch (error) {
    console.log('保存材料树失败', error)
  } finally {
    materialEditLoading.value = false
  }
}

// 保存其他费用分类
async function saveOther() {
  try {
    otherEditLoading.value = true
    const ids = otherTreeRef.value.getCheckedNodes(false, true).map((row) => row.id)
    await saveAmortizationClass({
      amortizationClassEnum: amortizationTypeEnum.OTHER_EXPENSES.V,
      ids
    })
    emit('success')
    otherTree.value = filterTree(JSON.parse(JSON.stringify(otherTree.value)), ids)
    otherEdit.value = false
  } catch (error) {
    console.log('保存其他费用失败', error)
  } finally {
    otherEditLoading.value = false
  }
}

// 过滤树
function filterTree(tree = [], ids) {
  return tree.filter((row) => {
    if (ids.includes(row.id)) {
      row.children = filterTree(row.children || [], ids)
      return true
    }
    return false
  })
}
</script>

<style lang="scss" scoped>
::v-deep(.el-card) {
  flex: 1;
  .el-card__header {
    padding: 10px;
    color: #fff;
    .icons {
      float: right;
      font-size: 20px;
      cursor: pointer;
      .icon {
        width: 1em;
        height: 1em;
        margin-left: 6px;
        vertical-align: 0;
      }
    }
  }
  .el-tree {
    overflow-y: auto;
  }
  &.amortization-tree {
    margin-right: 20px;
    .el-card__header {
      background: #0079ff;
    }
  }
  &.other-tree {
    .el-card__header {
      background: #36ae81;
    }
  }
}
</style>
