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
            :style="{ height: maxHeight + 'px' }"
            :data="
              materialEdit
                ? materialTree
                : (expenseClassEnumKV[expenseClassEnum.MATERIAL_AUXILIARY.V] &&
                    expenseClassEnumKV[expenseClassEnum.MATERIAL_OTHER.V] && [
                      expenseClassEnumKV[expenseClassEnum.MATERIAL_AUXILIARY.V],
                      expenseClassEnumKV[expenseClassEnum.MATERIAL_OTHER.V],
                    ]) ||
                  []
            "
            :props="defaultProps"
            :show-checkbox="materialEdit"
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
              <el-edit v-else class="icon" @click="edit(false)" />
            </div>
          </template>
          <el-tree
            ref="otherTreeRef"
            v-loading="otherLoading"
            :style="{ height: maxHeight + 'px' }"
            :data="
              otherEdit
                ? otherTree
                : expenseClassEnumKV[expenseClassEnum.OTHER_EXPENSES.V] && [expenseClassEnumKV[expenseClassEnum.OTHER_EXPENSES.V] || []]
            "
            :props="defaultProps"
            :show-checkbox="otherEdit"
            node-key="id"
            highlight-current
            default-expand-all
          />
        </el-card>
      </div>
      <common-table :data="describeData" :max-height="360" class="describe-data" style="margin-top: 20px">
        <el-table-column key="index" type="index" label="序号" align="center" width="60" />
        <el-table-column align="left" key="content" prop="content" label="摊销说明 (以下描述都以月表示摊销时间段)">
          <template #default="{ row }">
            <span style="white-space: pre-wrap;" v-html="row.content" />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { amortizationClassTree } from '@/api/contract/expense-entry/amortization-manage'
import { saveAmortizationClass } from '@/api/contract/expense-entry/amortization-manage'
import { ref, defineEmits, defineProps, inject } from 'vue'

import { expenseClassEnum } from '@enum-ms/contract'
import { getChildIds } from '@/utils/data-type/tree'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { ElMessage } from 'element-plus'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const expenseClassEnumKV = inject('expenseClassEnumKV')

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
  label: 'name',
  disabled: 'disabled'
})
const describeData = ref([
  {
    content: '产量 = 制成品累计入库的总量。'
  },
  {
    content: ' 项目摊销比例 = 项目的当月产量 / 当月所有项目的产量。'
  },
  {
    content: '摊销存在手动摊销与自动摊销两种类型。'
  },
  {
    content: '手动摊销：水电费、气体统计这两种类型的摊销为手动摊销。需要相关在信息填报后，由操作人手动进行摊销操作。'
  },
  {
    content: '自动摊销：材料费用、其它（报销）费用、厂房折旧等其它分类皆为自动摊销，系统将于每月底23:59:59自动进行摊销操作。'
  },
  {
    content: `材料费用 及 其它费用 
       （1）摊销分类需要通过“材料摊销分类设置” 与 “费用摊销分类设置”。
       （2）只有不以项目为名义的出库及申报才会进行摊销，否则直接计入对应项目费用。
       （3）摊销对应的时间是 当月在系统中进行材料出库的时间 与 费用填报的时间（而不是填写的报销日期等）。`
  },
  {
    content: `材料摊销分类设置 与 费用摊销分类设置
       （1）只有在配置中添加了的分类，该分类才会在月末进行摊销计算。
       （2）添加新的分类，新分类摊销从当月月初开始计算。
       （3）移除旧的分类，于当时直接生效。
       （4）分类仅包含一级分类。`
  },
  {
    content: '若所摊销的月份没有可摊销的产量，会将该月份需要摊销的金额并入下个月当中进行摊销。'
  },
  {
    content: ' 已结算项目的产量不会参与摊销。'
  }
])

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.amortization-setting-drawer',
    extraBox: ['.el-drawer__header', '.el-card__header', '.describe-data'],
    wrapperBox: ['.el-drawer__body'],
    extraHeight: 68,
    minHeight: 200
  },
  visible
)

// 编辑
function edit(val) {
  if (val) {
    materialEdit.value = true
    getMaterialTree()
  } else {
    otherEdit.value = true
    getOtherTree()
  }
}

// 获取材料分类的全部数据
async function getMaterialTree() {
  try {
    materialLoading.value = true
    const data =
      (await amortizationClassTree({ expenseClassEnum: expenseClassEnum.MATERIAL_AUXILIARY.V + expenseClassEnum.MATERIAL_OTHER.V })) || []
    data?.forEach((row) => {
      row.disabled = true
    })
    materialTree.value = data
    // 回显
    const tree = []
    if (expenseClassEnumKV.value[expenseClassEnum.MATERIAL_AUXILIARY.V]?.children?.length) {
      tree.push(...expenseClassEnumKV.value[expenseClassEnum.MATERIAL_AUXILIARY.V].children)
    }
    if (expenseClassEnumKV.value[expenseClassEnum.MATERIAL_OTHER.V]?.children?.length) {
      tree.push(...expenseClassEnumKV.value[expenseClassEnum.MATERIAL_OTHER.V].children)
    }
    materialTreeRef.value.setCheckedKeys(getChildIds(tree))
  } catch (error) {
    console.log('获取材料树失败', error)
  } finally {
    materialLoading.value = false
  }
}

// 获取其他费用分类的全部数据
async function getOtherTree() {
  try {
    otherLoading.value = true
    const data = (await amortizationClassTree({ expenseClassEnum: expenseClassEnum.OTHER_EXPENSES.V })) || []
    data?.forEach((row) => {
      row.disabled = true
    })
    otherTree.value = data
    otherTreeRef.value.setCheckedKeys(getChildIds(expenseClassEnumKV.value[expenseClassEnum.OTHER_EXPENSES.V]?.children))
  } catch (error) {
    console.log('获取其他费用树失败', error)
  } finally {
    otherLoading.value = false
  }
}

// 保存材料分类
async function saveMaterial() {
  try {
    materialEditLoading.value = true
    const params = [
      {
        expenseClassEnum: expenseClassEnum.MATERIAL_AUXILIARY.V,
        ids: []
      },
      {
        expenseClassEnum: expenseClassEnum.MATERIAL_OTHER.V,
        ids: []
      }
    ]
    const ids = materialTreeRef.value.getCheckedNodes(false, true).map((row) => row.id)
    filterTree(materialTree.value, ids, params)

    await saveAmortizationClass(params)
    ElMessage.success('保存成功')
    emit('success')
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
    await saveAmortizationClass([
      {
        expenseClassEnum: expenseClassEnum.OTHER_EXPENSES.V,
        ids
      }
    ])
    ElMessage.success('保存成功')
    emit('success')
    otherEdit.value = false
  } catch (error) {
    console.log('保存其他费用失败', error)
  } finally {
    otherEditLoading.value = false
  }
}

// 过滤树
function filterTree(tree = [], ids, params) {
  return tree.filter((row) => {
    if (ids.includes(row.id)) {
      row.children = filterTree(row.children || [], ids, params)
      if (row.expenseClassEnum === expenseClassEnum.MATERIAL_AUXILIARY.V) {
        params[0].ids.push(row.id)
      } else if (row.expenseClassEnum === expenseClassEnum.MATERIAL_OTHER.V) {
        params[1].ids.push(row.id)
      }
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
    .el-tree-node__content > label.el-checkbox {
      height: 26px;
    }
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
