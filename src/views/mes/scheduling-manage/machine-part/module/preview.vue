<template>
  <common-dialog
    customClass="machine-part-scheduling-preview-dlg"
    title="零件排产预览"
    v-model="dialogVisible"
    width="1100px"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button @click="submitIt" :loading="submitLoading" size="mini" type="primary">保存</common-button>
    </template>
    <div class="head-container">
      <!-- <common-select
        v-model="layWayConfigId"
        :options="layingWayList"
        :loading="layingWayLoading"
        :dataStructure="{ key: 'id', label: 'layingOffWayName', value: 'id' }"
        clearable
        class="filter-item"
        placeholder="请选择下料方式"
        style="width: 200px"
      >
        <template #view="{ data }">
          <span class="customize-option-item">
            <span class="label">{{ data.layingOffWayName }}</span>
            <span>
              <span class="extra-label">
                <span v-parse-enum="{ e: materialTypeEnum, v: data.materialType }"></span>
              </span>
            </span>
          </span>
        </template>
      </common-select> -->
      <el-form style="display: flex; flex-wrap: wrap">
        <el-form-item label="材质：" class="form-label-require">
          <common-select
            v-model="material"
            :options="materialList"
            :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
            clearable
            filterable
            allow-create
            type="other"
            class="filter-item"
            placeholder="请选择材质"
            style="width: 160px"
          />
        </el-form-item>
        <el-form-item label="厚度：" class="form-label-require">
          <common-select
            v-model="thick"
            :options="thickList"
            :dataStructure="{ key: 'name', label: 'name', value: 'name' }"
            clearable
            filterable
            allow-create
            type="other"
            class="filter-item"
            placeholder="请选择厚度"
            style="width: 160px"
          />
        </el-form-item>
        <!-- <el-form-item label="生产组：" class="form-label-require">
          <el-cascader
            :ref="(el) => (cascaderRef[$index] = el)"
            v-model="groupsId"
            :options="groupsTree"
            :props="{
              value: 'id',
              label: 'name',
              children: 'children',
              expandTrigger: 'hover',
              emitPath: false,
            }"
            :show-all-levels="false"
            filterable
            clearable
            :placeholder="$index === 0 ? '请选择生产组' : '同上'"
            @expand-change="handleExpandChange($event, $index, cascaderRef[$index])"

            @change="handleGroupsChange($event, $index)"
          />
        </el-form-item> -->
        <el-form-item label="排产日期：" class="form-label-require">
          <el-date-picker
            v-model="dateTime"
            type="date"
            size="small"
            class="date-item filter-item"
            style="width: 160px !important"
            placeholder="选择排产日期"
            :clearable="false"
            format="YYYY-MM-DD"
            value-format="YYYY-MM-DD"
            :disabled-date="disabledDate"
          />
        </el-form-item>
      </el-form>
    </div>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="project" label="所属项目" min-width="120px" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度(mm)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" :label="`单净重(kg)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" width="80" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { save } from '@/api/mes/scheduling-manage/machine-part'
import { ElNotification } from 'element-plus'
import { defineEmits, defineProps, ref, inject } from 'vue'
// import { materialTypeEnum } from '@enum-ms/uploading-form'
// import useSchedulingGroups from '@compos/mes/scheduling/use-scheduling-groups'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  },
  materialList: {
    type: Array,
    default: () => []
  },
  thickList: {
    type: Array,
    default: () => []
  }
})

const dataFormat = ref([['project', 'parse-project']])

// const layWayConfigId = ref()
// const layingWayList = ref([])
// const layingWayLoading = ref(false)
const submitLoading = ref(false)
const dateTime = ref()
const groupsId = ref()
const crud = inject('crud')

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
// const { groupsTree, groupsObj } = useSchedulingGroups({ queryParams, factoryIds: curFactoryIds })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.machine-part-scheduling-preview-dlg',
    extraBox: ['.el-dialog__header', 'head-container'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

// 生产组级联
// function handleFocusChange(expend, row, index, curCascaderRef) {
//   if (!row.groupsId || row.groupsId === '同上') {
//     const menus = curCascaderRef.panel.menuList[0]
//     setCascaderExpandNode(menus, menus.nodes[0])
//   }
// }

function setCascaderExpandNode(menus, node) {
  if (!node.isLeaf) {
    menus.panel.expandNode(node, true)
    if (!node.children[0].isLeaf) {
      setCascaderExpandNode(menus, node.children[0])
    }
  }
}

function handleExpandChange(expend, row, index, curCascaderRef) {
  const menus = curCascaderRef.panel.menuList[0]
  const curExpandingNode = menus.panel.expandingNode
  if (!curExpandingNode.children[0].isLeaf) {
    setCascaderExpandNode(menus, curExpandingNode.children[0])
  }
}

// async function fetchLayingWay() {
//   try {
//     layWayConfigId.value = undefined
//     layingWayLoading.value = true
//     const content = await getLayingWay()
//     layingWayList.value = content.filter((v) => {
//       if (crud.query.thickList === '其他') {
//         return v.materialType === materialTypeEnum.MANMADE_BLANKING.V
//       } else {
//         return true
//       }
//     })
//   } catch (error) {
//     console.log('获取下料方式列表', error)
//   } finally {
//     layingWayLoading.value = false
//   }
// }

function disabledDate(time) {
  return time < new Date()
}
async function submitIt() {
  try {
    // if (!layWayConfigId.value) {
    //   ElMessage.warning('请选择下料方式')
    //   return
    // }
    submitLoading.value = true
    const _list = []
    props.list.forEach((v) => {
      v.needMachinePartLinkList.forEach((o) => {
        _list.push({
          productId: v.id,
          quantity: o.quantity,
          id: o.id,
          needSchedulingMonth: o.date
        })
      })
    })
    await save({
      // layWayConfigId: layWayConfigId.value,
      material: crud.query.material,
      thickList: crud.query.thickList,
      linkList: _list
    })
    ElNotification({
      title: '零件排产保存成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('保存零件排产报错', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
