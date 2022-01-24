<template>
  <common-dialog title="分配预览" v-model="dialogVisible" fullscreen :before-close="handleClose" top="5vh" :center="false">
    <template #titleAfter>
      <el-tooltip
        effect="light"
        :content="`预览定义：\n
                1. /：表示该项未修改；
                2. 当构件存在协同任务时，默认减少所操作生产线对应班组的任务数量，
        如有其他需求或当前生产线对应班组任务数量不足，请在班组协同页面修改协同任务数量。\n`"
        placement="right"
      >
        <span>
          <i class="el-icon-info" />
        </span>
      </el-tooltip>
    </template>
    <template #titleRight>
      <el-popover placement="left-end" width="850" trigger="click">
        <common-table :data="workshopList">
          <el-table-column width="150" property="name" label="车间" />
          <el-table-column width="110" align="center" property="increase" label="新增数量" />
          <template v-if="productType & (componentTypeEnum.ENCLOSURE.V | componentTypeEnum.ASSEMBLE.V)">
            <el-table-column min-width="110" align="center" property="lengthIncrease" :label="`新增分配长度\n（m）`">
              <template v-slot="scope">
                {{ convertUnits(scope.row.lengthIncrease, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
              </template>
            </el-table-column>
          </template>
          <template v-else>
            <el-table-column width="110" align="center" property="netWeightIncrease" :label="`新增分配重量\n（净重, kg）`">
              <template v-slot="scope">
                {{ toFixed(scope.row.netWeightIncrease, DP.COM_WT__KG) }}
              </template>
            </el-table-column>
            <el-table-column width="110" align="center" property="grossWeightIncrease" :label="`新增分配重量\n（毛重, kg）`">
              <template v-slot="scope">
                {{ toFixed(scope.row.grossWeightIncrease, DP.COM_WT__KG) }}
              </template>
            </el-table-column>
          </template>

          <!-- <el-table-column width="110" align="center" property="reduce" label="减少数量" />
          <template v-if="productType & (componentTypeEnum.ENCLOSURE.V | componentTypeEnum.ASSEMBLE.V)">
            <el-table-column min-width="110" align="center" property="lengthReduce" :label="`减少分配长度\n（m）`">
              <template v-slot="scope">
                {{ convertUnits(scope.row.lengthReduce, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
              </template>
            </el-table-column>
          </template>
          <template v-else>
            <el-table-column width="110" align="center" property="netWeightReduce" :label="`减少分配重量\n（净重, kg）`">
              <template v-slot="scope">
                {{ toFixed(scope.row.netWeightReduce, DP.COM_WT__KG) }}
              </template>
            </el-table-column>
            <el-table-column width="110" align="center" property="grossWeightReduce" :label="`减少分配重量\n（毛重, kg）`">
              <template v-slot="scope">
                {{ toFixed(scope.row.grossWeightReduce, DP.COM_WT__KG) }}
              </template>
            </el-table-column>
          </template> -->
        </common-table>
        <template #reference>
          <common-button type="warning" size="mini">查看分配汇总</common-button>
        </template>
      </el-popover>
      <common-button :loading="loading" size="mini" :disabled="!modifiedData || modifiedData.length == 0" type="primary" @click="submit">
        保 存
      </common-button>
    </template>
    <common-table :data="modifiedData" :max-height="maxHeight" empty-text="未做改动" style="width: 100%">
      <el-table-column fixed label="序号" type="index" align="center" width="60" />
      <productType-full-info-columns
        :productType="productType"
        :category="category"
        :unShowField="[
          'netWeight',
          'grossWeight',
          'totalNetWeight',
          'totalGrossWeight',
          'surfaceArea',
          'weight',
          'totalArea',
          'totalLength',
          'drawingNumber',
          'remark',
        ]"
      />
      <template v-for="workshop in lines">
        <template v-for="line in workshop.productionLineList">
          <el-table-column
            v-if="line.changed"
            :key="line.id"
            :prop="`productionLine_${line.id}`"
            :label="line.name"
            align="center"
            width="150px"
          >
            <template v-slot:header>
              <span class="ellipsis-text">【{{ workshop.name }}】</span>
              <span class="ellipsis-text">{{ line.name }}</span>
              <el-tooltip class="item" effect="light" :content="ellipsisTextTip(workshop, line)" placement="top-start">
                <div>
                  <span
v-if="changedLineData[line.id]"
style="color: #11b95c"
                    >▲{{ changedLineData[line.id].increase }}&nbsp;&nbsp;&nbsp;</span
                  >
                  <span v-if="changedLineData[line.id]" style="color: red">▼{{ changedLineData[line.id].reduce }}</span>
                </div>
              </el-tooltip>
            </template>
            <template v-slot="scope">
              <template v-if="scope.row.schedulingMap[line.id]">
                <span>{{ scope.row.sourceSchedulingMap[line.id].quantity || 0 }}</span>
                ▶
                <span>{{ scope.row.schedulingMap[line.id].quantity || 0 }}</span
                >&nbsp;&nbsp;&nbsp;[
                <span :style="{ color: scope.row.schedulingMap[line.id].changeQuantity > 0 ? '#11b95c' : 'red' }">{{
                  scope.row.schedulingMap[line.id].changeQuantity > 0
                    ? ` ↑${scope.row.schedulingMap[line.id].changeQuantity} `
                    : ` ↓${-scope.row.schedulingMap[line.id].changeQuantity} `
                }}</span>
                ]
              </template>
              <span v-else>/</span>
            </template>
          </el-table-column>
        </template>
      </template>
      <el-table-column min-width="1px" />
      <el-table-column fixed="right" prop="unassignQuantity" label="未分配" align="center" width="120px">
        <template v-slot:header>
          <span>未分配</span>
          <span v-if="changeQuantity > 0" style="color: red">▲{{ changeQuantity }}</span>
          <span v-if="changeQuantity < 0" style="color: #11b95c">▼{{ -changeQuantity }}</span>
        </template>
        <template v-slot="scope">
          <span>{{ scope.row.sourceUnassignQuantity }}</span>
          <span
            >▶<span :style="{ color: scope.row.unassignQuantity < scope.row.sourceUnassignQuantity ? '#11b95c' : 'red' }">{{
              scope.row.unassignQuantity
            }}</span></span
          >
        </template>
      </el-table-column>
      <el-table-column fixed="right" prop="assignQuantity" label="已分配" align="center" width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.sourceAssignQuantity }}</span>
          <span
            >▶<span :style="{ color: scope.row.assignQuantity > scope.row.sourceAssignQuantity ? '#11b95c' : 'red' }">{{
              scope.row.assignQuantity
            }}</span></span
          >
        </template>
      </el-table-column>
      <el-table-column fixed="right" prop="quantity" label="数量" align="center" width="70px" />
    </common-table>
    <template #footer>
      <div class="dialog-footer"></div>
    </template>
  </common-dialog>
</template>

<script setup>
import { save } from '@/api/mes/scheduling-manage/scheduling/common'
import { defineProps, defineEmits, ref, watch, inject } from 'vue'
import { ElNotification } from 'element-plus'

import { componentTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type'
import { obj2arr } from '@/utils/convert/type'
import { convertUnits } from '@/utils/convert/unit'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import productTypeFullInfoColumns from '@comp-mes/table-columns/productType-full-info-columns'

const emit = defineEmits(['update:visible', 'success'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  data: {
    type: Array,
    default: () => []
  },
  lines: {
    type: Array,
    default: () => []
  }
})

const productType = inject('productType')
const category = inject('category', undefined)
const processType = inject('processType')
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

const modifiedData = ref([])
const loading = ref(false)
const changedLineData = ref({})
const changeQuantity = ref(0)
const workshopList = ref([])

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      handleDataChange()
    }
  },
  { immediate: true }
)

function ellipsisTextTip(workshop, line) {
  const _data = changedLineData.value[line.id]
  if (productType & (componentTypeEnum.ENCLOSURE.V | componentTypeEnum.ASSEMBLE.V)) {
    return `${workshop.name}\n
                ${line.name}\n
                新增分配数量：${_data.increase} 张\n
                新增分配长度：${convertUnits(_data.lengthIncrease, 'mm', 'm', DP.MES_ENCLOSURE_L__M)} m\n`
  } else {
    return `${workshop.name}\n
                ${line.name}\n
                新增分配数量：${_data.increase} 件\n
                新增分配重量（净重）：${toFixed(_data.netWeightIncrease, DP.COM_WT__KG)} kg\n
                新增分配重量（毛重）：${toFixed(_data.grossWeightIncrease, DP.COM_WT__KG)} kg\n`
  }
}

async function submit() {
  try {
    loading.value = true
    // 数据格式处理
    const list = []
    modifiedData.value.forEach((i) => {
      const schedulingList = obj2arr(i.schedulingMap)
      // 数量不存在则填写为0
      schedulingList.forEach((s) => {
        s.schedulingQuantity = s.quantity || 0
        list.push({
          productId: i.id,
          projectId: i.projectId,
          areaId: i.areaId,
          monomerId: i.monomerId,
          productType: productType,
          processType: processType,
          schedulingQuantity: s.schedulingQuantity - (s.sourceQuantity || 0),
          factoryId: s.factoryId,
          workshopId: s.workshopId,
          productionLineId: s.productionLineId
        })
      })
    })
    await save({ schedulingList: list })
    handleClose()
    emit('success', list)
    ElNotification({ title: '任务分配成功', type: 'success' })
  } catch (error) {
    console.log('任务分配提交', error)
  } finally {
    loading.value = false
  }
}

function handleDataChange() {
  const _data = JSON.parse(JSON.stringify(props.data))
  const hasChangedLine = {}
  const hasChangedWorkshop = {}
  changeQuantity.value = 0
  modifiedData.value = _data.filter((v) => {
    const schedulingMap = v.schedulingMap
    const sourceSchedulingMap = v.sourceSchedulingMap
    changeQuantity.value += (v.unassignQuantity || 0) - (v.sourceUnassignQuantity || 0)
    if (schedulingMap) {
      for (const key in schedulingMap) {
        const changeQuantity = (schedulingMap[key].quantity || 0) - (sourceSchedulingMap[key].quantity || 0)
        if (changeQuantity === 0) {
          // 过滤未修改的数据
          delete schedulingMap[key]
        } else {
          // 任务数量有改变，生产线累加任务的变化数量
          schedulingMap[key].changeQuantity = changeQuantity
          if (!hasChangedLine[key]) {
            hasChangedLine[key] = {
              increase: 0,
              reduce: 0,
              netWeightIncrease: 0,
              netWeightReduce: 0,
              grossWeightIncrease: 0,
              grossWeightReduce: 0,
              lengthIncrease: 0,
              lengthReduce: 0
            }
          }
          if (changeQuantity > 0) {
            hasChangedLine[key].increase += Math.abs(changeQuantity)
            if (productType & (componentTypeEnum.ENCLOSURE.V | componentTypeEnum.ASSEMBLE.V)) {
              hasChangedLine[key].lengthIncrease += Math.abs(changeQuantity) * v.length
            } else {
              hasChangedLine[key].netWeightIncrease += Math.abs(changeQuantity) * v.netWeight
              hasChangedLine[key].grossWeightIncrease += Math.abs(changeQuantity) * v.grossWeight
            }
          } else {
            hasChangedLine[key].reduce += Math.abs(changeQuantity)
            if (productType & (componentTypeEnum.ENCLOSURE.V | componentTypeEnum.ASSEMBLE.V)) {
              hasChangedLine[key].lengthReduce += Math.abs(changeQuantity) * v.length
            } else {
              hasChangedLine[key].netWeightReduce += Math.abs(changeQuantity) * v.netWeight
              hasChangedLine[key].grossWeightReduce += Math.abs(changeQuantity) * v.grossWeight
            }
          }
        }
      }
      if (Object.keys(schedulingMap) && Object.keys(schedulingMap).length > 0) {
        return true
      } else {
        return false
      }
    } else {
      return false
    }
  })
  const lineIds = Object.keys(hasChangedLine)
  props.lines.forEach((workshop) => {
    const productionLineList = workshop.productionLineList || []
    productionLineList.forEach((line) => {
      line.changed = lineIds.includes(`${line.id}`)
      if (line.changed) {
        // 如果生产线任务有改变，工厂累加生产线变化的量
        if (!hasChangedWorkshop[workshop.id]) {
          hasChangedWorkshop[workshop.id] = {
            name: workshop.name,
            increase: 0,
            reduce: 0,
            netWeightIncrease: 0,
            netWeightReduce: 0,
            grossWeightIncrease: 0,
            grossWeightReduce: 0,
            lengthIncrease: 0,
            lengthReduce: 0
          }
        }
        hasChangedWorkshop[workshop.id].increase += hasChangedLine[line.id].increase
        hasChangedWorkshop[workshop.id].reduce += hasChangedLine[line.id].reduce
        if (productType & (componentTypeEnum.ENCLOSURE.V | componentTypeEnum.ASSEMBLE.V)) {
          hasChangedWorkshop[workshop.id].lengthIncrease += hasChangedLine[line.id].lengthIncrease
          hasChangedWorkshop[workshop.id].lengthReduce += hasChangedLine[line.id].lengthReduce
        } else {
          hasChangedWorkshop[workshop.id].netWeightIncrease += hasChangedLine[line.id].netWeightIncrease
          hasChangedWorkshop[workshop.id].netWeightReduce += hasChangedLine[line.id].netWeightReduce
          hasChangedWorkshop[workshop.id].grossWeightIncrease += hasChangedLine[line.id].grossWeightIncrease
          hasChangedWorkshop[workshop.id].grossWeightReduce += hasChangedLine[line.id].grossWeightReduce
        }
      }
    })
  })
  changedLineData.value = hasChangedLine
  workshopList.value = obj2arr(hasChangedWorkshop)
}
</script>

<style lang="scss" scoped>
::v-deep(.el-dialog__body) {
  padding-top: 0;
}
</style>
