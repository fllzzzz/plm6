<template>
  <common-drawer
    ref="drawerRef"
    title="查看零件余料"
    custom-class="section-steel-detail"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    :size="1000"
  >
    <template #content>
      <div class="item-name" style="float: left">钢板信息</div>
      <el-descriptions :column="2" border>
        <el-descriptions-item label="项目/单体" label-align="center" align="center">
          {{ detailData.projectName }} <span v-if="detailData.monomer"> {{detailData.monomer }}</span>
        </el-descriptions-item>
        <el-descriptions-item label="切割指令号" label-align="center" align="center" label-class-name="my-label" class-name="my-content">
          {{ detailData.cutInstructionId }}
        </el-descriptions-item>
        <el-descriptions-item label="规格（mm）" label-align="center" align="center">
          {{ detailData.thick + ' X ' + detailData.width + '*' + detailData.length }}
        </el-descriptions-item>
        <el-descriptions-item label="材质" label-align="center" align="center">
          <el-tag size="small">{{ detailData.material }}</el-tag>
        </el-descriptions-item>
        <!-- IMG -->
        <el-descriptions-item style="width: 200px; height: 100px" label="钢板图形" label-align="center" align="center">
          <el-image
            style="width: 100%; height: 100%"
            :preview-src-list="detailData.srcList"
            :src="detailData.platePictureUrl"
            fit="cover"
          />
        </el-descriptions-item>
      </el-descriptions>

      <div class="item-name" style="float: left">零件信息</div>
      <!--表格渲染-->
      <common-table ref="tableRef" :data="partData" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" min-width="60" type="index" />
        <el-table-column prop="specification" label="规格" align="center" min-width="100"></el-table-column>
        <el-table-column prop="length" label="长度（mm）" align="center" min-width="100"></el-table-column>
        <el-table-column prop="material" label="材质" align="center" min-width="100"></el-table-column>
        <el-table-column prop="quantity" label="数量" align="center" min-width="100"></el-table-column>
        <el-table-column prop="netWeight" label="单净重（kg）" align="center" min-width="100"></el-table-column>
        <el-table-column prop="grossWeight" label="单毛重（kg）" align="center" min-width="100"></el-table-column>
        <el-table-column prop="totalNetWeight" label="总净重（kg）" align="center" min-width="100"></el-table-column>
        <el-table-column prop="totalGrossWeight" label="总毛重（kg）" align="center" min-width="100"></el-table-column>
      </common-table>
      <div class="item-name" style="float: left">余料信息</div>
      <!--表格渲染-->
      <common-table ref="tableRef" style="width: 100%" :data="CutSurplusData">
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column prop="specification" label="规格" align="center" min-width="100"></el-table-column>
        <el-table-column prop="material" label="材质" align="center" min-width="100"></el-table-column>
        <el-table-column prop="weight" label="重量（kg）" align="center" min-width="100"></el-table-column>
        <el-table-column prop="num" label="数量" align="center" min-width="100"></el-table-column>
        <el-table-column prop="area" label="面积" align="center" min-width="100"></el-table-column>
        <el-table-column label="图形" align="center" min-width="100">
          <template v-slot="scope">
            <el-image style="width: 100%; height: 100%" :src="scope.row.surplusPictureUrl" fit="cover" />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref } from 'vue'
// import { steelPlateEnum } from '@enum-ms/cutting'
import { getCutPart, getCutSurplus } from '@/api/cutting/project-data'

const emit = defineEmits(['update:visible'])
const partData = ref([]) // 零件数据
const CutSurplusData = ref([]) // 余料数据
const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object,
    required: true
  }
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  CutPart()
  CutSurplus()
}

async function CutPart() {
  partData.value = await getCutPart(props.detailData.taskId)
  if (partData.value === '没有零件') {
    partData.value = []
  }
}

async function CutSurplus() {
  CutSurplusData.value = await getCutSurplus(props.detailData.taskId)
  CutSurplusData.value.forEach(item => {
    item.platePictureUrlList = []
    item.platePictureUrlList.push(item.platePictureUrl)
  })
}

</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
.qwe {
  width: 640px;
  height: 155px;
  // background-color: red;
}
</style>

