<template>
  <el-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    :visible="dlgVisible"
    top="15vh"
    fullscreen
    width="500px"
  >
    <template slot="title">
      <el-tooltip
        effect="light"
        :content="`配套件添加规则：\n
          1、类别：必填；\n
          2、规格：选填，不能超过60个字符；\n
          3、数量：必填，不能超过${maxNubmer}；\n
          3、核算量：必填，不能超过${maxNubmer}；\n
          5、备注：选填，不能超过500个字符；\n
          `"
        placement="right"
      >
        <div style="display:inline-block;">
          <span>新增配套件清单</span>
          <i class="el-icon-info" style="color:#909399" />
        </div>
      </el-tooltip>
    </template>
    <el-form
      ref="form"
      :model="form"
      :rules="rules"
      size="small"
      label-width="90px"
    >
      <el-table
        ref="table"
        :data="form.list"
        :cell-class-name="handelCellClassName"
        max-height="600"
        style="width: 100%"
        :border="$TBS.BORDER"
      >
        <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
        <el-table-column prop="serialNumber" align="center" label="编号" min-width="100">
          <template v-slot="scope">
            <el-tag v-if="scope.row.serialNumber" size="medium">{{ scope.row.serialNumber }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column label="*物料种类" prop="materialClass" align="center" min-width="330">
          <template v-slot="scope">
            <material-class-cascader
              ref="materialClassCascader"
              :value.sync="scope.row.materialClass"
              :basic-class="basicClassEnum.MATERIAL.V"
              filterable
              show-unit
              show-code
              @change="handleMaterialChange(scope.row)"
            />
          </template>
        </el-table-column>
        <!--        <el-table-column prop="color" label="颜色" align="center" min-width="110">-->
        <!--          <template v-slot="scope">-->
        <!--            <el-input v-model="scope.row.color" size="small" placeholder="颜色" />-->
        <!--          </template>-->
        <!--        </el-table-column>-->
        <el-table-column prop="specification" label="规格" align="center" width="200">
          <template v-slot="scope">
            <el-input v-model="scope.row.specification" size="small" placeholder="规格" />
          </template>
        </el-table-column>
        <el-table-column prop="unit" label="单位" align="center" width="200">
          <template v-slot="scope">
            <span>{{ scope.row.unit | emptyTextFormatter }}</span>
            <!--            <el-input v-model="scope.row.unit" size="small" placeholder="规格" />-->
          </template>
        </el-table-column>
        <el-table-column prop="quantity" label="数量" align="center" min-width="160">
          <template v-slot="scope">
            <el-input-number v-model="scope.row.quantity" :min="1" :max="maxNubmer" :step="1" :precision="0" size="small" controls-position="right" />
          </template>
        </el-table-column>
        <el-table-column prop="unit" label="核算单位" align="center" width="200">
          <template v-slot="scope">
            <span>{{ scope.row.unit | emptyTextFormatter }}</span>
            <!--            <el-input v-model="scope.row.unit" size="small" placeholder="规格" />-->
          </template>
        </el-table-column>
        <el-table-column prop="quantity" label="核算量" align="center" min-width="160">
          <template v-slot="scope">
            <el-input-number v-model="scope.row.quantity" :min="1" :max="maxNubmer" :step="1" :precision="0" size="small" controls-position="right" />
          </template>
        </el-table-column>
        <el-table-column prop="remark" label="备注" align="center" min-width="200">
          <template v-slot="scope">
            <el-input v-model="scope.row.remark" size="small" placeholder="备注" />
          </template>
        </el-table-column>
        <el-table-column label="操作" align="center">
          <template v-slot="scope">
            <el-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
          </template>
        </el-table-column>
      </el-table>
      <div class="footer-drawer">
        <el-button class="cancel-btn" size="small" icon="el-icon-circle-plus-outline" type="warning" style="margin-right:15px" @click="addRow()">继续添加</el-button>
      </div>
    </el-form>
    <div slot="footer" class="dialog-footer">
      <el-button type="text" @click="handleClose">取消</el-button>
      <el-button :loading="loading" type="primary" @click="submit">确认</el-button>
    </div>
  </el-dialog>
</template>

<script>
import { add } from '@/api/mes-plan/technical-manage/auxiliary-material'
import materialClassCascader from '@/views/components/base/material-class-cascader'
import { materialBasicClassEnum as basicClassEnum } from '@/utils/enum/index'
import { tableValidate } from '@/utils/other'

const maxNubmer = 999999999

export default {
  components: { materialClassCascader },
  props: {
    visible: {
      type: Boolean,
      default: false
    }
  },
  data() {
    return {
      loading: false,
      basicClassEnum,
      maxNubmer,
      form: {},
      rules: {
        quantity: [{ required: true, message: '请填写辅材数量', trigger: 'blur', type: 'number' }],
        materialClass: [{ required: true, message: '请选择辅材类别', trigger: 'change' }],
        specification: [{ max: 60, message: '不能超过60个字符', trigger: 'blur' }],
        color: [{ max: 30, message: '不能超过30个字符', trigger: 'blur' }],
        remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
      }
    }
  },
  computed: {
    dlgVisible() {
      return this.visible
    }
  },
  watch: {
    visible(newVal) {
      if (newVal) {
        // this.form.areaId = this.areaId
        this.form.list = []
        this.addRow()
      }
    }
  },
  methods: {
    handleClose() {
      this.$emit('update:visible', false)
    },
    // [CRUD.HOOK.afterToAdd]() {
    //   this.form.areaId = this.crud.query.areaId
    //   this.form.list = []
    //   this.addRow()
    // },
    // [CRUD.HOOK.beforeSubmit]() {
    //   console.log(this.crud.status.add === this.prepared)
    //   if (this.crud.status.add === this.prepared) {
    //     if (!this.form.areaId) {
    //       this.$message({ message: '请在列表页面选择区域', type: 'error' })
    //     }
    //     return !!(this.form.areaId && this.validate())
    //   } else {
    //     return true
    //   }
    // },
    async submit() {
      try {
        this.loading = true
        if (this.validate()) {
          await add(this.form)
          this.$emit('success', this.form)
          this.handleClose()
        }
      } catch (error) {
        console.log('辅材添加', error)
      } finally {
        this.loading = false
      }
    },
    validate() {
      let flag = true
      const list = this.form.list
      let message = '请填写数据'
      if (list && list.length > 0) {
        // TODO: 考虑封装
        list.forEach(row => {
          for (const rule in this.rules) {
            row.verify[rule] = tableValidate(this.rules[rule], row[rule])
            if (!row.verify[rule]) {
              flag = false
            }
          }
        })
        if (!flag) {
          this.form.list = Object.assign([], list)
          message = '请修正表格中标红的信息, 填写限制请参照标题旁边的“信息符号(i)”'
        }
      } else {
        flag = false
      }
      if (!flag) {
        this.$message({ message, type: 'error' })
      }
      return flag
    },
    handelCellClassName({ row, column, rowIndex, columnIndex }) {
      let flag = true
      if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
        if (row.verify[column.property] === false) {
          flag = tableValidate(this.rules[column.property], row[column.property])
        }
        if (flag) {
          row.verify[column.property] = true
        }
      }
      return flag ? '' : 'mask-td'
    },
    handleMaterialChange: function(row) {
      row.thirdId = row.materialClass[2]
      row.serialNumber = this.$refs.materialClassCascader.getSerialNumber()
    },
    // 添加行
    addRow: function() {
      this.form.list.push({ verify: {}})
    },
    // 删除行
    deleteRow: function(index) {
      this.form.list.splice(index, 1)
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  .footer-drawer {
    display: flex;
    flex-direction: row;
    justify-content: center;
    align-items: center;
    margin-top: 20px;
  }
</style>
