// graphics library
// standard way of creating windows, widgets (CSS/react box model) via arrkia FML

// 2D: always use vectors. Convert the result into FML and a gpu shader/command buffer. Which can then be passed to the GPU to render into a frame. Once done, the command buffer should tell the GPU to output that frame to the display (including prev static data)
// Always antialias diagonals

class Window {}

class Widget {}

// always draw to the opposite "side"
// all children inherit their parent's attributes
enum PositionStart {
    Set1 = (0, 0)
    Set2 = (1, 1)
    Set3 = (0, 1)
    Set4 = (1, 0)
}

@target(os=Neutron)
const POSITION_START = Set1;

// always the 'top left' (0, 0)
// can be changed via PositionStart
class Position {
    
}

class Box {
    color: Color
    z_index: i64
    position: Position

}

class Color {
    r, g, b: (u8, u8, u8)
    alpha: u8

    // auto generated
    // actually means fn Color(...) -> Self {}
    // if there is a function called the same thing it can be used as a constructor
    // No need for initialisation lists
    fn Color(r, g, b: (u8, u8, u8), alpha: u8) {
        // call super
        super()

        // actually returns Self with fields initialised in the initialisation list
        Self {
            r, g, b, alpha
        }
    }

    // factory method

    fn rgb(&self) -> (u8,u8,u8) {
        return self.(r, g, b)
    }

    fn alpha(&self) -> u8 {
        return self.alpha
    }

    // unlike rust, &mut self is always passed?? Nah
}

fn main() {
    let c = Color()
}

// Dragging and Resizing API

use std::graphics::resize
use std::graphics::drag

// by default neutron/arcwm has its own library of widgets built on rei widgets
